{-# LANGUAGE ScopedTypeVariables #-}

module Graphics where

import           Control.Monad          (forM_, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text
import           Foreign.C.Types
import           GHC.Word               (Word32)
import           Reflex.SDL2
import qualified SDL.Font
import qualified SDL.Image

windowWidth :: CInt
windowWidth = 800

windowHeight :: CInt
windowHeight = 600

worldWidth :: CInt
worldWidth = 10000

worldHeight :: CInt
worldHeight = 10000

textureDimensions :: CInt
textureDimensions = 32

maxFrames :: Word32
maxFrames = 1000

data TilesInScreen = TilesInScreen {
  _horizontalTilesNumber :: !Int,
  _verticalTilesNumber   :: !Int
}

data SDLTexture = SDLTexture {
  _getSDLTexture :: !Texture,
  _sizeT         :: !(V2 CInt)
} deriving (Eq)

data Textures = Textures {
  _humanM :: !SDLTexture,
  _humanF :: !SDLTexture,
  _soil   :: !SDLTexture,
  _grass  :: !SDLTexture,
  _stone  :: !SDLTexture,
  _water  :: !SDLTexture
} deriving (Eq)

loadTextures :: Renderer -> IO Textures
loadTextures r = do
  humanMale <- loadTexture r "assets/human_male.png"
  humanFemale <- loadTexture r "assets/human_female.png"
  soil <- loadTexture r "assets/soil.png"
  grass <- loadTexture r "assets/grass.png"
  stone <- loadTexture r "assets/stone.png"
  water <- loadTexture r "assets/water.png"
  return $ Textures humanMale humanFemale soil grass stone water

loadTexture :: Renderer -> FilePath -> IO SDLTexture
loadTexture r filePath = do
  (texture, info) <- loadTextureWithInfo r filePath
  let size = V2 (textureWidth info) (textureHeight info)
  return $ SDLTexture texture size

destroyTextures :: Textures -> IO ()
destroyTextures t = do
  destroyTexture $ _getSDLTexture $ _humanM t
  destroyTexture $ _getSDLTexture $ _humanF t
  destroyTexture $ _getSDLTexture $ _soil t
  destroyTexture $ _getSDLTexture $ _grass t
  destroyTexture $ _getSDLTexture $ _stone t
  destroyTexture $ _getSDLTexture $ _water t

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
      size = V2 windowWidth windowHeight
      ogl = defaultOpenGL{ glProfile = Core Normal 3 3 }
      cfg = defaultWindow{ windowOpenGL      = Just ogl
                          , windowResizable   = False
                          , windowHighDPI     = False
                          , windowInitialSize = size
                          }

withRenderer :: (MonadIO m) => Window -> (Renderer -> m a) -> m ()
withRenderer w op = do
  r <- createRenderer w (-1) (RendererConfig AcceleratedRenderer True)
  rendererDrawColor r $= V4 130 72 38 255
  -- rendererDrawBlendMode r $= BlendAlphaBlend
  void $ op r
  destroyRenderer r

withTextures :: (MonadIO m) => Renderer -> ((Renderer, Textures) -> m a) -> m ()
withTextures r op = do
    t <- liftIO $ loadTextures r
    void $ op (r, t)
    liftIO $ destroyTextures t

loadTextureWithInfo :: (MonadIO m) => Renderer -> FilePath -> m (Texture, TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- queryTexture t
  pure (t, i)

mkPoint :: a -> a -> Point V2 a
mkPoint x y = P (V2 x y)

fromPointCIntToVectorDouble :: Point V2 CInt -> V2 Double
fromPointCIntToVectorDouble (P (V2 x y)) = V2 (fromIntegral x) (fromIntegral y)

fromPointDoubleToPointCInt :: Point V2 Double -> Point V2 CInt
fromPointDoubleToPointCInt (P (V2 x y)) = P $ V2 (round x) (round y)

mkRect :: a -> a -> a -> a-> Rectangle a
mkRect x y w h = Rectangle o z
  where
    o = P (V2 x y)
    z = V2 w h

renderGrid :: (MonadIO m) => Renderer -> [(SDLTexture, Point V2 CInt)] -> m ()
renderGrid r texturesAndPositions =
  -- let soil = _soil t
  -- renderTextureInPositions r (soil, backgroundTexturesPositions 0 0) -- render grid background
  forM_ texturesAndPositions (renderTextureInPosition r)
  -- liftIO $ putStrLn $ "renderGrid end"

renderTextureInPosition :: (MonadIO m) => Renderer -> (SDLTexture , Point V2 CInt) -> m ()
renderTextureInPosition r (t,postion) =
  liftIO $ renderTexture r t postion

renderTextureInPositions :: (MonadIO m) => Renderer -> (SDLTexture , [Point V2 CInt]) -> m ()
renderTextureInPositions r (t, postions) =
  liftIO $ forM_ postions (renderTexture r t)

backgroundTexturesPositions :: CInt -> CInt -> [Point V2 CInt]
backgroundTexturesPositions x y
  | y > windowHeight = []
  | x > windowWidth  = P (V2 0 y) : backgroundTexturesPositions 0 (y+textureDimensions)
  | otherwise        = P (V2 x y) : backgroundTexturesPositions (x+textureDimensions) y

renderTexture :: Renderer -> SDLTexture -> Point V2 CInt -> IO ()
renderTexture r (SDLTexture t size) xy =
  Reflex.SDL2.copy r t Nothing (Just $ Rectangle xy size)

renderSolidText :: MonadIO m => Renderer -> SDL.Font.Font ->
  SDL.Font.Color -> String -> Int -> Int -> m ()
renderSolidText r font =
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
