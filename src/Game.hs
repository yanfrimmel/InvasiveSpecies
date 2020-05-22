{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Game (app) where

import           Control.Category
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Foreign.C.Types
import           Graphics
import           Input
import           Prelude              hiding (id, (.))
import           Reflex
import           Reflex.SDL2
import           System.Random
import           Time
import           Types
import           Utils
import           Data.Label as L

worldWidth :: CInt
worldWidth = 10000

worldHeight :: CInt
worldHeight = 10000

-- | A type representing one layer in our app.
type Layer m = Performable m ()
----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.
-- commitLayers :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
--       => Dynamic t [Layer m] -> m ()
-- commitLayers = tellDyn

----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure

app :: IO ()
app = liftIO $ withSDL $ withSDLImage $ withSDLFont $
    withWindow "InvasiveSpecies" $ \world ->
     withRenderer world $ \renderer ->
      withTextures renderer $ \(r, t) ->
       host $ runReaderT appReader (r, t)
       where
         appReader = do
          (r, _) <- ask
          (_, dynLayers) <- runDynamicWriterT game
          performEvent_ $ ffor (updated dynLayers) $ \layers -> do
            clear r
            sequence_ layers
            present r
          evQuit <- getQuitEvent
          performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
          shutdownOn =<< delay 0 evQuit

type ReflexSDLMonadsContainer t m = (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)

game :: (ReflexSDLMonadsContainer t m, MonadReader (Renderer, Textures) m) => m ()
game = do
  _ <- getPostBuild
  gameTimeDyn <- createGameFrameTimeDynamic -- set up time and limit values
  deltaDyn <- holdDyn (createTime 0) (ffilter _isGameFrame (updated gameTimeDyn))   -- Filter out non-game ticks
  deltaCountDyn <- count $ updated deltaDyn  -- Count when delta fires and compare at different times to calculate fps
  fpsDyn <- createTickOnceASecondDynamic deltaCountDyn -- Tick once a second to calculate FPS
  performEvent_ $ fmap fpsPrint (updated fpsDyn) -- Print a message every frame tick
  renderGameGrid deltaCountDyn fpsDyn

createGameFrameTimeDynamic :: (ReflexSDL2 t m) => m (Dynamic t Time)
createGameFrameTimeDynamic = do
  tickEvent <- getDeltaTickEvent
  limitDyn <- holdDyn maxFrames never
  foldDyn updateTime (createTime maxFrames) (attachPromptlyDyn limitDyn tickEvent)

createTickOnceASecondDynamic :: (ReflexSDL2 t m) =>  Dynamic t Int -> m (Dynamic t Int)
createTickOnceASecondDynamic deltaCount = do
  secondCount <- tickLossyFromPostBuildTime 1
  deltaStore <- foldDyn (\ a (b, _) -> (a, b)) (0, 0) $
                  tagPromptlyDyn deltaCount secondCount
  holdDyn 0 $ uncurry (-) <$> updated deltaStore

renderGameGrid :: (ReflexSDLMonadsContainer t m , MonadReader (Renderer, Textures) m) => Dynamic t Int -> Dynamic t Int -> m ()
renderGameGrid deltaCountDyn fpsDyn = do
  (r, textures) <- ask
  let defaultMouseButton = MouseButtonEventData Nothing Released (Mouse 0) ButtonRight 0 (P $ V2 0 0)
  let defaultMouseMotion = MouseMotionEventData Nothing (Mouse 0) [] (P $ V2 0 0) (V2 0 0)
  mouseClickDyn <- holdDyn defaultMouseButton =<< getMouseButtonEvent
  performEvent_ $ fmap printMouseClick (updated mouseClickDyn)
  mouseMotionDyn <- holdDyn defaultMouseMotion =<< getMouseMotionEvent
  let mouseInputDyn = zipDyn mouseClickDyn mouseMotionDyn

  let initialGameState = GameState {
      _camera = initialCameraPosition,
      _player = GameObject {
          _id = 1,
          _speed = 200,
          _texture =  _humanM textures,
          _position = initialPlayerPosition,
          _gameObjectType = Player
      },
      _gameObjects = [GameObject{
                                 _id = 2,
                                 _speed = 0,
                                 _texture = _humanF textures,_position = P (V2 5100 5100),
                                 _gameObjectType = initialHumanFemale
                      }]
      }
  let gameInputsDyn = zipDynWith putMouseAndFpsEventIntoInput fpsDyn mouseInputDyn
  let inputUpdateEvent = tagPromptlyDyn gameInputsDyn (updated deltaCountDyn)
  gameStateDyn <- foldDyn inputEventHandler initialGameState inputUpdateEvent
  -- let gameObjectsInputDyn = foldDyn updateGameObjects 0 (return [GameObject]) (updated gameStateDyn)

  commitLayer $ ffor deltaCountDyn $ \_ -> do
    newState <- sample $ current gameStateDyn
    -- printMessage $ "Player" ++ show (_player newState)
    renderGrid r $ createGameStateView newState

  showFPSOnScreenOnceASecond r fpsDyn

inputEventHandler :: Input -> GameState -> GameState
inputEventHandler i g =
  if isLeftButtonDown (fst $ _mouseInput i)
    then
      updatePlayerPosition (updateAnimalPosition delta (fromPointCIntToPointCFloat mouseWorldPos) (_player g)) g
  else if isInfinite delta -- TODO: better solution for first second FPS better
    then g    
  else
    updateGameObjects (mkStdGen 0) delta g -- TODO: put another value into generator
  where
    mouseWorldPos = getMousePosition (snd $ _mouseInput i) ^+^ _camera g
    delta = 1 / fromIntegral (_currentFPS i) :: CFloat

updateAnimalPosition :: CFloat -> Point V2 CFloat -> GameObject -> GameObject
updateAnimalPosition delta target a = 
  if distanceBetweenPoints > proximityThreshold 
    then L.set position (P newPosition) a
  else a
  where 
    s = _speed a
    currPos = _position a
    direction = normalize $ fromPointToVector target ^-^ fromPointToVector currPos
    newPosition = fromPointToVector currPos ^+^ (direction ^* (s * delta))
    distanceBetweenPoints = distance (fromPointToVector currPos) (fromPointToVector target)
    proximityThreshold = fromIntegral textureDimensions / 8 

initialPlayerPosition :: Point V2 CFloat
initialPlayerPosition = P $ fromPointCIntToVectorCFloat $ P (V2 (div worldWidth 2) (div worldHeight 2))

initialCameraPosition :: Point V2 CInt
initialCameraPosition = fromPointCFloatToPointCInt initialPlayerPosition ^-^ P (V2 (div windowWidth 2) (div windowHeight 2))

putMouseAndFpsEventIntoInput :: Int -> (MouseButtonEventData, MouseMotionEventData) -> Input
putMouseAndFpsEventIntoInput fps m = Input{ _currentFPS = fps, _mouseInput = m }

showFPSOnScreenOnceASecond :: ReflexSDLMonadsContainer t m => Renderer -> Dynamic t Int -> m ()
showFPSOnScreenOnceASecond r fpsDyn = do
  rf <- regularFont
  commitLayer $ ffor fpsDyn $ \a ->
    renderSolidText r rf (V4 255 255 0 255) ("FPS: " ++ show a) 0 0

fpsPrint :: MonadIO m => Int -> m ()
fpsPrint fps = printMessage $ "FPS: " ++ show fps

printMouseClick :: MonadIO m => MouseButtonEventData -> m ()
printMouseClick m = printMessage $ "new position: " ++ show (getMouseButtonClickPosition m)

printMessage :: MonadIO m => String -> m ()
printMessage m = liftIO $ putStrLn m

updatePlayerPosition :: GameObject -> GameState -> GameState
updatePlayerPosition p s =
  s{  _player = p,
      _camera = fromPointCFloatToPointCInt newPosition - P (V2 (div windowWidth 2) (div windowHeight 2))
   }
    where
      newPosition = calcPlayerPosition (_position p)

calcPlayerPosition :: Point V2 CFloat -> Point V2 CFloat
calcPlayerPosition (P (V2 x y))
  | x < 0 && y < 0                    = P (V2 0 0)
  | x < 0                             = P (V2 0 y)
  | y < 0                             = P (V2 x 0)
  | x > fromIntegral worldWidth && y > fromIntegral worldHeight = P (V2 (fromIntegral worldWidth) (fromIntegral worldHeight))
  | x > fromIntegral worldWidth                    = P (V2 (fromIntegral worldWidth) y)
  | y > fromIntegral worldHeight                   = P (V2 x (fromIntegral worldHeight))
  | otherwise                         = P (V2 x y)

createGameStateView :: GameState -> [(SDLTexture, Point V2 CInt)]
createGameStateView s = createGameObjectsView (_camera s) (_player s : _gameObjects s)

createGameObjectsView :: Point V2 CInt -> [GameObject] -> [(SDLTexture, Point V2 CInt)]
createGameObjectsView cam gameObjs =
  transformGameObjectsPoisitionToFrame (filter (checkIfTextureInFrame cam) fromGameObjsToTexturePoints)
  where
    fromGameObjsToTexturePoints = map (\ gameObj -> (_texture gameObj, fromPointCFloatToPointCInt $ _position gameObj )) gameObjs
    transformGameObjectsPoisitionToFrame = map (\ (t, p) -> (t, p - cam))