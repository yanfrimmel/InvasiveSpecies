{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Game (app) where

import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Foreign.C.Types
import           Graphics
import           Input
import           Reflex
import           Reflex.SDL2
import           Time
import           Types
import           Utils

worldWidth :: CInt
worldWidth = 10000

worldHeight :: CInt
worldHeight = 10000

-- | A type representing one layer in our app.
type Layer m = Performable m ()
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

game :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m) => m ()
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

inputEventHandler :: Inputs -> GameState -> GameState
inputEventHandler i g =
  if isLeftButtonDown (fst $ _mouseInput i) &&
    distanceBetweenPoints > proximityThreshold
    then
      updatePlayerPosition (P newPositon) g
  else
    g
  where
    mouseWorldPos = getMousePosition (snd $ _mouseInput i) ^+^ _camera g
    speed = _speed (_player g)
    currPos = _position (_player g)
    elapsed = 1 / fromIntegral (_currentFPS i)
    direction = normalize $ fromPointCIntToVectorCFloat mouseWorldPos ^-^ fromPointToVector currPos
    newPositon = fromPointToVector currPos ^+^ (direction ^* (speed * elapsed))
    distanceBetweenPoints = distance (fromPointToVector currPos) (fromPointCIntToVectorCFloat mouseWorldPos)
    proximityThreshold = fromIntegral textureDimensions / 8

renderGameGrid :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m) => Dynamic t Int -> Dynamic t Int -> m ()
renderGameGrid deltaCountDyn fpsDyn = do
  (r, textures) <- ask
  let defaultMouseButton = MouseButtonEventData Nothing Released (Mouse 0) ButtonRight 0 (P $ V2 0 0)
  let defaultMouseMotion = MouseMotionEventData Nothing (Mouse 0) [] (P $ V2 0 0) (V2 0 0)
  mouseClickDyn <- holdDyn defaultMouseButton =<< getMouseButtonEvent
  -- performEvent_ $ fmap (printMouseClick) (updated mouseClickDyn)
  mouseMotionDyn <- holdDyn defaultMouseMotion =<< getMouseMotionEvent
  let mouseInputDyn = zipDyn mouseClickDyn mouseMotionDyn

  let initialGameState = GameState {
      _camera = initialCameraPosition,
      _player = GameObject {
          _id = 1,
          _speed = 100,
          _texture =  _humanM textures,
          _position = initialPlayerPosition
                           },
      _gameObjects = [GameObject{
                                 _id = 2,
                                 _speed = 0,
                                 _texture = _humanF textures,_position = P (V2 5100 5100)
                                }
                     ]}
  let gameInputsDyn = zipDynWith putMouseAndFpsEventIntoInputs fpsDyn mouseInputDyn
  let inputUpdateEvent = tagPromptlyDyn gameInputsDyn (updated deltaCountDyn)
  gameStateDyn <- foldDyn inputEventHandler initialGameState inputUpdateEvent

  commitLayer $ ffor deltaCountDyn $ \_ -> do
    newState <- sample $ current gameStateDyn
    printMessage $ "Player pos: " ++ (show $ _position $ _player newState)
    renderGrid r $ createGameStateView newState

  showFPSOnScreenOnceASecond r fpsDyn

initialPlayerPosition :: Point V2 CFloat
initialPlayerPosition = P $ fromPointCIntToVectorCFloat $ P (V2 (div worldWidth 2) (div worldHeight 2))

initialCameraPosition :: Point V2 CInt
initialCameraPosition = fromPointCFloatToPointCInt initialPlayerPosition ^-^ P (V2 (div windowWidth 2) (div windowHeight 2))

putMouseAndFpsEventIntoInputs :: Int -> (MouseButtonEventData, MouseMotionEventData) -> Inputs
putMouseAndFpsEventIntoInputs fps m = Inputs{_currentFPS = fps, _mouseInput = m}

showFPSOnScreenOnceASecond :: (MonadSample t (Performable m), ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m) => Renderer -> Dynamic t Int -> m ()
showFPSOnScreenOnceASecond r fpsDyn = do
  rf <- regularFont
  commitLayer $ ffor fpsDyn $ \a ->
    renderSolidText r rf (V4 255 255 0 255) ("FPS: " ++ show a) 0 0

-- Function to just print something to the screen
fpsPrint :: MonadIO m => Int -> m ()
fpsPrint fps = liftIO $ putStrLn $ "FPS: " ++ show fps

-- Function to just print something to the screen
printMouseClick :: MonadIO m => MouseButtonEventData -> m ()
printMouseClick m = printMessage $ "new position: " ++ show (getMouseButtonClickPosition m)

printMessage :: MonadIO m => String -> m ()
printMessage m = liftIO $ putStrLn m

updatePlayerPosition :: Point V2 CFloat -> GameState -> GameState
updatePlayerPosition p s =
  s{  _player = (_player s){_position = newPosition},
      _camera = fromPointCFloatToPointCInt newPosition - P (V2 (div windowWidth 2) (div windowHeight 2))
   }
    where
      newPosition = calcPlayerPosition p

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
createGameObjectsView camera gameObjs =
  transformGameObjectsPoisitionToFrame (filter (checkIfGameObjectInFrame camera) fromGameObjsToTexturePoints)
  where
    fromGameObjsToTexturePoints = map (\ gameObj -> (_texture gameObj, fromPointCFloatToPointCInt $ _position gameObj )) gameObjs
    transformGameObjectsPoisitionToFrame = map (\ (t, p) -> (t, p - camera))

checkIfGameObjectInFrame :: Point V2 CInt -> (SDLTexture, Point V2 CInt) -> Bool
checkIfGameObjectInFrame (P (V2 x y)) (_, P (V2 x2 y2))
 | x2 - x < 0           = False
 | y2 - y < 0           = False
 | x2 - x > worldWidth  = False
 | y2 - y > worldHeight = False
 | otherwise            = True
