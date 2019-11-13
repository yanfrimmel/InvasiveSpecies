{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Game where

import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Foreign.C.Types
import GHC.Word(Word32)
import Graphics
import Reflex
import Reflex.SDL2
import Input

-- | A type representing one layer in our app.
type Layer m = Performable m ()

-- _gameObjectsView extracted from _gameObjects
data GameState = GameState {
  _player :: !GameObject,
  _gameObjects :: [GameObject]
} deriving (Eq)

data GameObject = GameObject {
  _id :: !Int,
  _speed :: Int,
  _position :: !(Point V2 CInt),
  _texture :: !SDLTexture
} deriving (Eq)
----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.
commitLayers :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
      => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn

----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure

app :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadReader (Renderer, Textures) m) => m ()
app = do
  (r, t) <- ask
  (_, dynLayers) <- runDynamicWriterT game
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    clear r
    sequence_ layers
    present r     
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit

game
  :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m)
  => m ()
game = do
  gameReady <- getPostBuild
  (r, textures) <- ask
  -- Set up time and limit values
  ticks <- getDeltaTickEvent
  limit <- holdDyn maxFrames never
  unfTime <- foldDyn updateTime (createTime maxFrames) (attachPromptlyDyn limit ticks)
  -- Filter out non-game ticks
  delta <- holdDyn (createTime 0) (ffilter _nextFrame (updated unfTime))
  -- Count when delta fires and compare at different times to calculate fps
  deltaCount <- count $ updated delta
  -- Tick every second to calculate FPS
  secondCount <- tickLossyFromPostBuildTime 1
  deltaStore <- foldDyn (\a (b,_)->(a,b)) (0,0) $ tagPromptlyDyn deltaCount secondCount
  fps <- holdDyn 0 $ uncurry (-) <$> updated deltaStore
  -- Print a message every frame tick
  performEvent_ $ fmap (fpsPrint) (updated fps)
  -- render game
  updateGame r textures deltaCount
  -- Show FPS on screen sceen once a second
  rf <- regularFont 
  commitLayer $ ffor fps $ \a -> 
    renderSolidText r rf (V4 255 255 0 255) ("FPS: " ++ show a) 0 0
  ----------------------------------

gameStateDynamicInitalize :: (ReflexSDL2 t m) => Inputs -> Event t MouseButtonEventData -> m (Dynamic t Inputs)
gameStateDynamicInitalize i e = foldDyn fromMouseEventToInputs i e

fromMouseEventToInputs :: MouseButtonEventData -> Inputs -> Inputs
fromMouseEventToInputs m i = i {_mouseButtonEventData = m}
  
inputEventHandler :: Inputs -> GameState -> GameState  
inputEventHandler i g =    
  if (isLeftButtonIsDown $ _mouseButtonEventData i) && currPos /= mousePos
    then
      upadatePlayerPosition mousePos g
  else 
    g 
  where 
    mousePos = getMousePosition $ _mouseButtonEventData i
    currPos = _position (_player g)  
    -- dis = distance (unP currPos) (unP mousePos)

updateGame :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m) => Renderer -> Textures -> Dynamic t Integer -> m ()
updateGame r textures  deltaCount = do
  defaultMouseButton <- return $ MouseButtonEventData Nothing Released (Mouse 0) ButtonLeft 0 (P $ V2 0 0)
  mouseClickDyn <- holdDyn defaultMouseButton =<< getMouseButtonEvent

  let initialGameState = GameState {_player = GameObject {_id = 1, _speed = 3, _texture =  _humanM textures, _position = (P (V2 0 0)) }, _gameObjects = []}
  let initialInput = Inputs {_mouseButtonEventData = defaultMouseButton } 
  gameInputsByMouseClickDyn <- gameStateDynamicInitalize initialInput (updated mouseClickDyn)
  -- TODO: attach other than mouse input events
  let inputUpdateEvent = tagPromptlyDyn gameInputsByMouseClickDyn (updated deltaCount)
  stateDynVal <- foldDyn inputEventHandler initialGameState inputUpdateEvent
  
  commitLayer $ ffor deltaCount $ \a -> do 
    newState <- sample $ current stateDynVal
    printMessage $ showPositionsInState $ head (createGameStateView newState)
    renderGrid r textures (createGameStateView newState)  

-- Function to just print something to the screen
fpsPrint :: MonadIO m => Integer -> m ()
fpsPrint fps = liftIO $ putStrLn $ "FPS: " ++ show fps

-- Function to just print something to the screen
onMouseClick :: MonadIO m => MouseButtonEventData -> m ()
onMouseClick m = printMessage $ "new position: " ++ show (getMousePosition m)

printMessage :: MonadIO m => String -> m ()
printMessage m = liftIO $ putStrLn $ m

showPositionsInState :: (SDLTexture, Point V2 CInt) -> String
showPositionsInState (texture, position) = "showPositionsInState: " ++ show position

upadatePlayerPosition :: Point V2 CInt -> GameState -> GameState
upadatePlayerPosition p s =  
  (s { _player = (_player s)  {
            _position = p
          }
        } 
    )

createGameStateView :: GameState -> [(SDLTexture, Point V2 CInt)]
createGameStateView s = creategGameObjectsView (_player s : _gameObjects s)

creategGameObjectsView :: [GameObject] -> [(SDLTexture, Point V2 CInt)]
creategGameObjectsView objs = map (\o -> (_texture o, _position o)) objs