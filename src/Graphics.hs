module Graphics where

import           Reflex.SDL2
import qualified SDL.Image
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO) 
import           Foreign.C.Types
import           Data.Text

data TilesInScreen = TilesInScreen {
  _horizontalTilesNumber :: !Int,
  _verticalTilesNumber :: !Int,
}

data SDLTexture = SDLTexture { getSDLTexture :: Texture
                       , sizeT         :: V2 CInt
                       }

data Textures = Textures { _humanM    :: !SDLTexture
, _humanF    :: !SDLTexture
, _soil    :: !SDLTexture
, _grass    :: !SDLTexture
, _stones     :: !SDLTexture
}

loadTextures :: Renderer -> IO Textures
loadTextures r =
  Textures
    <$> loadTexture r "assets/human_male.png"
    <*> loadTexture r "assets/human_female.png"
    <*> loadTexture r "assets/soil.png"
    <*> loadTexture r "assets/grass.png"
    <*> loadTexture r "assets/stones.png"

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  initializeAll
  void op
  quit

withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (Window -> m a) -> m ()
withWindow title (x, y) op = do      
  w <- createWindow title cfg
  showWindow w
  void $ glCreateContext w
  void $ op w
  destroyWindow w
    where
      size = V2 (fromIntegral x) (fromIntegral y)
      ogl = defaultOpenGL{ glProfile = Core Debug 3 3 }
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

loadTexture :: Renderer -> FilePath -> IO SDLTexture
loadTexture r filePath = do
  surface <- SDL.Image.load filePath
  size <- surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  surfaceColorKey surface $= Just key
  t <- createTextureFromSurface r surface
  freeSurface surface
  return $ SDLTexture t size              

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

