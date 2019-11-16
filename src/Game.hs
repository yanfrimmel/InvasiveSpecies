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
import Input
import Reflex
import Reflex.SDL2
import Time

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

game :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m) => m ()
game = do
  gameReady <- getPostBuild
  gameTimeDyn <- createGameFrameTimeDynamic -- set up time and limit values
  deltaDyn <- holdDyn (createTime 0) (ffilter _nextFrame (updated gameTimeDyn))   -- Filter out non-game ticks
  deltaCountDyn <- count $ updated deltaDyn  -- Count when delta fires and compare at different times to calculate fps
  fpsDyn <- createTickOnceASecondDynamic deltaCountDyn -- Tick once a second to calculate FPS
  performEvent_ $ fmap (fpsPrint) (updated fpsDyn) -- Print a message every frame tick
  renderGameGrid deltaCountDyn fpsDyn

createGameFrameTimeDynamic :: (ReflexSDL2 t m) => m (Dynamic t Time) 
createGameFrameTimeDynamic = do
  tickEvent <- getDeltaTickEvent
  limitDyn <- holdDyn maxFrames never
  unfTimeDyn <- foldDyn updateTime (createTime maxFrames) (attachPromptlyDyn limitDyn tickEvent)
  return unfTimeDyn

createTickOnceASecondDynamic :: (ReflexSDL2 t m) =>  Dynamic t Integer -> m (Dynamic t Integer)
createTickOnceASecondDynamic deltaCount = do
  secondCount <- tickLossyFromPostBuildTime 1
  deltaStore <- foldDyn (\a (b,_)->(a,b)) (0,0) $ tagPromptlyDyn deltaCount secondCount
  fpsDyn <- holdDyn 0 $ uncurry (-) <$> updated deltaStore
  return fpsDyn
  
inputEventHandler :: Inputs -> GameState -> GameState  
inputEventHandler i g =    
  if (isLeftButtonDown $ fst $ _mouseInput i) && distanceBetweenPoints > proximityThreshold
    then
      upadatePlayerPosition (fromPointDoubleToPointCInt $ P newPositon) g
  else 
    g 
  where 
    mousePos = getMousePosition $ snd $ _mouseInput i
    speed = _speed (_player g)
    currPos = _position (_player g)
    elapsed = 1 / fromIntegral (_currentFPS i)
    direction = normalize $ (fromPointCIntToVectorDouble mousePos) ^-^ (fromPointCIntToVectorDouble currPos)
    newPositon = (fromPointCIntToVectorDouble currPos) ^+^ (direction ^* (fromIntegral speed * elapsed))
    distanceBetweenPoints = distance (fromPointCIntToVectorDouble currPos) (fromPointCIntToVectorDouble mousePos) 
    proximityThreshold = (fromIntegral textureDimensions) / 8

renderGameGrid :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m) => Dynamic t Integer -> Dynamic t Integer -> m ()
renderGameGrid deltaCountDyn fpsDyn = do
  (r, textures) <- ask
  let defaultMouseButton = MouseButtonEventData Nothing Released (Mouse 0) ButtonRight 0 (P $ V2 0 0)
  let defaultMouseMotion = MouseMotionEventData Nothing (Mouse 0) [] (P $ V2 0 0) (V2 0 0)
  mouseClickDyn <- holdDyn defaultMouseButton =<< getMouseButtonEvent
  mouseMotionDyn <- holdDyn defaultMouseMotion =<< getMouseMotionEvent
  let mouseInputDyn = zipDyn mouseClickDyn mouseMotionDyn
  
  let initialGameState = GameState {_player = GameObject {_id = 1, _speed = 1000, _texture =  _humanM textures, _position = (P (V2 0 0)) }, _gameObjects = []}
  let initialInput = Inputs {_currentFPS = 1, _mouseInput = (defaultMouseButton, defaultMouseMotion) } 
  let gameInputsDyn = zipDynWith putMouseAndFpsEventIntoInputs fpsDyn mouseInputDyn
  let inputUpdateEvent = tagPromptlyDyn gameInputsDyn (updated deltaCountDyn)
  gameStateDyn <- foldDyn inputEventHandler initialGameState inputUpdateEvent
  
  commitLayer $ ffor deltaCountDyn $ \deltaCount -> do 
    newState <- sample $ current gameStateDyn
    -- printMessage $ showPositionsInState $ head (createGameStateView newState)
    -- printMessage $ "deltaCount: " ++ show deltaCount
    renderGrid r textures (createGameStateView newState)  

  showFPSOnScreenOnceASecond r fpsDyn   

putMouseAndFpsEventIntoInputs :: Integer -> (MouseButtonEventData, MouseMotionEventData) -> Inputs
putMouseAndFpsEventIntoInputs fps m = Inputs{_currentFPS = fps, _mouseInput = m}

showFPSOnScreenOnceASecond :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m) => Renderer -> Dynamic t Integer -> m ()
showFPSOnScreenOnceASecond r fpsDyn = do 
  rf <- regularFont 
  commitLayer $ ffor fpsDyn $ \a -> 
    renderSolidText r rf (V4 255 255 0 255) ("FPS: " ++ show a) 0 0
    
-- Function to just print something to the screen
fpsPrint :: MonadIO m => Integer -> m ()
fpsPrint fps = liftIO $ putStrLn $ "FPS: " ++ show fps

-- Function to just print something to the screen
onMouseClick :: MonadIO m => MouseButtonEventData -> m ()
onMouseClick m = printMessage $ "new position: " ++ show (getMouseButtonClickPosition m)

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