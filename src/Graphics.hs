{-# LANGUAGE ScopedTypeVariables   #-}

module Graphics where

import           Control.Monad          (void, forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Data.List
import           Data.Text 
import           Foreign.C.Types
import           GHC.Word(Word32)
import           Reflex.SDL2
import qualified SDL.Font
import qualified SDL.Image

screenWidth :: CInt
screenWidth = 800

screenHeight :: CInt
screenHeight = 600

textureDimensions :: CInt 
textureDimensions = 32

maxFrames :: Word32
maxFrames = 60

data TilesInScreen = TilesInScreen {
  _horizontalTilesNumber :: !Int,
  _verticalTilesNumber :: !Int
}

data Time = Time { 
  _elapsed :: !Word32,
  _frameLimit  :: !Word32,
  _nextFrame   :: !Bool,
  _postFrame   :: !Bool
} deriving (Eq, Show)

data SDLTexture = SDLTexture { 
  _getSDLTexture :: !Texture,
  _sizeT :: V2 CInt
} deriving (Eq)

data Textures = Textures { 
  _humanM    :: !SDLTexture,
  _humanF    :: !SDLTexture,
  _soil    :: !SDLTexture,
  _grass    :: !SDLTexture,
  _stones     :: !SDLTexture,
  _gridTexture :: !SDLTexture
} deriving (Eq)

loadTextures :: Renderer -> IO Textures
loadTextures r = do
  t <- createTexture r RGBA8888 TextureAccessTarget (V2 screenWidth screenHeight)
  Textures
    <$> loadTexture r "assets/human_male.png"
    <*> loadTexture r "assets/human_female.png"
    <*> loadTexture r "assets/soil.png"
    <*> loadTexture r "assets/grass.png"
    <*> loadTexture r "assets/stones.png"
    <*> (return $ SDLTexture t (V2 screenWidth screenHeight))   

loadTexture :: Renderer -> FilePath -> IO SDLTexture
loadTexture r filePath = do
  surface <- SDL.Image.load filePath
  size <- surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  surfaceColorKey surface $= Just key
  t <- createTextureFromSurface r surface
  freeSurface surface
  return $ SDLTexture t size
    
destroyTextures :: Textures -> IO ()
destroyTextures ts = do
  destroyTexture $ _getSDLTexture $ _humanM ts
  destroyTexture $ _getSDLTexture $ _humanF ts
  destroyTexture $ _getSDLTexture $ _soil ts
  destroyTexture $ _getSDLTexture $ _grass ts
  destroyTexture $ _getSDLTexture $ _stones ts
  destroyTexture $ _getSDLTexture $ _gridTexture ts

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  initializeAll
  void op
  quit

withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  void op
  SDL.Font.quit  

withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

withWindow :: (MonadIO m) => Text -> (Window -> m a) -> m ()
withWindow title op = do      
  w <- createWindow title cfg
  showWindow w
  void $ glCreateContext w
  void $ op w
  destroyWindow w
    where
      size = V2 screenWidth screenHeight
      ogl = defaultOpenGL{ glProfile = Core Normal 3 3 }
      cfg = defaultWindow{ windowOpenGL      = Just ogl
                          , windowResizable   = False
                          , windowHighDPI     = False
                          , windowInitialSize = size
                          }

withRenderer :: (MonadIO m) => Window -> (Renderer -> m a) -> m ()
withRenderer w op = do
  r <- createRenderer w (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  void $ op r
  destroyRenderer r    

withTextures :: (MonadIO m) => Renderer -> ((Renderer, Textures) -> m a) -> m ()
withTextures r op = do
    t <- liftIO $ loadTextures r
    void $ op (r, t)
    liftIO $ destroyTextures t      

renderSurfaceToWindow :: (MonadIO m) => Window -> Surface -> Surface -> m ()
renderSurfaceToWindow w s i
  = surfaceBlit i Nothing s Nothing
  >> updateWindowSurface w

loadTextureWithInfo :: (MonadIO m) => Renderer -> FilePath -> m (Texture, TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- queryTexture t
  pure (t, i)

mkPoint :: a -> a -> Point V2 a
mkPoint x y = P (V2 x y)

mkRect :: a -> a -> a -> a-> Rectangle a
mkRect x y w h = Rectangle o z
  where
    o = P (V2 x y)
    z = V2 w h 

renderGrid :: (MonadIO m) => Renderer -> Textures -> [(SDLTexture, [Point V2 CInt])] -> m ()
renderGrid r t texturesToPostions = do
  let grid = _gridTexture t
  let soil = _soil t
  rendererRenderTarget r $= Just (_getSDLTexture $ grid)
  forM_ (texturesToPostions) (renderTextureInPositions r) 
  renderTextureInPositions r (soil, (backgroundTexturesPositions 0 0)) -- render grid background
  rendererRenderTarget r $= Nothing -- render window
  Reflex.SDL2.copy r (_getSDLTexture $ grid) Nothing Nothing
  liftIO $ putStrLn $ "renderGrid end"

renderTextureInPositions :: (MonadIO m) => Renderer -> (SDLTexture , [Point V2 CInt]) -> m ()  
renderTextureInPositions r (t,postions) = do
  liftIO $ forM_ postions (renderTexture r t)

backgroundTexturesPositions :: CInt -> CInt -> [Point V2 CInt] 
backgroundTexturesPositions x y 
  | y > screenHeight = []
  | x > screenWidth  = P (V2 0 y) : backgroundTexturesPositions 0 (y+textureDimensions)
  | otherwise        = P (V2 x y) : backgroundTexturesPositions (x+textureDimensions) y

renderTexture :: Renderer -> SDLTexture -> Point V2 CInt -> IO ()
renderTexture r (SDLTexture t size) xy =
  Reflex.SDL2.copy r t Nothing (Just $ Rectangle xy size)

renderTextureRotated :: Renderer -> SDLTexture -> Point V2 CInt -> CDouble -> IO ()
renderTextureRotated r (SDLTexture t size) xy ang =
  copyEx r t Nothing (Just $ Rectangle xy size) ang Nothing (V2 False False)

renderRepeatedTexture :: Renderer -> SDLTexture -> CInt -> CInt -> IO ()
renderRepeatedTexture r t@(SDLTexture _ (V2 width height)) ox oy = do
  renderTexture r t (P (V2 offset (oy - height)))
  renderTexture r t (P (V2 (offset + width) (oy - height)))
  where
    offset = ox - (ox `div` width) * width - width

renderRepeatedTextureY :: Renderer -> SDLTexture -> CInt -> [CInt] -> IO ()
renderRepeatedTextureY r t ox xs =
  forM_ xs $ \oy ->
    renderTexture r t (P (V2 ox oy)) 
    
-- Easy way to create a Time
createTime :: Word32 -> Time
createTime limit = Time 0 limit True False

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (fLimt, delta) time =
  time
    { _elapsed = (\(a,_,_) -> a) check
    , _frameLimit = fLimt
    , _nextFrame = (\(_,a,_) -> a) check
    , _postFrame = (\(_,_,a)->a) check
    }
    where newAccum = _elapsed time + delta
          limit
            | _frameLimit time == 0 = 0
            | otherwise = round (1000 / fromIntegral (_frameLimit time))
          check
            | limit <= 0 = (delta, True, True)
            | _postFrame time = (mod newAccum limit, False, False)
            | newAccum > limit = (newAccum, True, True)
            | otherwise = (newAccum, False, False)    
           
renderSolidText :: MonadIO m => Renderer -> SDL.Font.Font -> 
  SDL.Font.Color -> String -> Int -> Int -> m ()
renderSolidText r font = do
  renderText r font (SDL.Font.solid font)            

renderText :: MonadIO m => Renderer -> SDL.Font.Font -> 
  (SDL.Font.Color -> Data.Text.Text -> m Surface) ->
  SDL.Font.Color -> String -> Int -> Int -> m ()
renderText r font fColor c str x y = do
    let text = Data.Text.pack str
    surface <- fColor c text
    texture <- Reflex.SDL2.createTextureFromSurface r surface
    Reflex.SDL2.freeSurface surface
    size <- SDL.Font.size font text
    let (w, h) = size
        x' :: CInt = fromIntegral x
        y' :: CInt = fromIntegral y
    Reflex.SDL2.copy r texture Nothing (Just (Rectangle (P $ V2 x' y') (V2 (fromIntegral w) (fromIntegral h))))
    Reflex.SDL2.destroyTexture texture

 -- Load a font from a file
getFontFromFile :: MonadIO m => FilePath -> Int -> m SDL.Font.Font
getFontFromFile path size = do
  liftIO $ putStrLn ("Loading font: " ++ show path) 
  SDL.Font.load path size   

regularFont :: MonadIO m => m SDL.Font.Font
regularFont = getFontFromFile "assets/ObliviousFont.ttf" 20  