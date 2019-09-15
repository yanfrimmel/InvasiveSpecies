module Graphics where

import           Reflex.SDL2
import qualified SDL.Image
import           Control.Monad          (void, forM_)
import           Control.Monad.IO.Class (MonadIO) 
import           Foreign.C.Types
import           Data.Text

screenDimensions :: V2 CInt 
screenDimensions = V2 800 600


data TilesInScreen = TilesInScreen {
  _horizontalTilesNumber :: !Int,
  _verticalTilesNumber :: !Int
}

data SDLTexture = SDLTexture { getSDLTexture :: Texture
                       , sizeT         :: V2 CInt
                       }

data Textures = Textures { _humanM    :: !SDLTexture
, _humanF    :: !SDLTexture
, _soil    :: !SDLTexture
, _grass    :: !SDLTexture
, _stones     :: !SDLTexture
, _gridTexture :: !SDLTexture
}

loadTextures :: Renderer -> IO Textures
loadTextures r = do
  t <- createTexture r RGBA8888 TextureAccessTarget screenDimensions
  Textures
    <$> loadTexture r "assets/human_male.png"
    <*> loadTexture r "assets/human_female.png"
    <*> loadTexture r "assets/soil.png"
    <*> loadTexture r "assets/grass.png"
    <*> loadTexture r "assets/stones.png"
    <*> (return $ SDLTexture t screenDimensions)   

loadTexture :: Renderer -> FilePath -> IO SDLTexture
loadTexture r filePath = do
  surface <- SDL.Image.load filePath
  size <- surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  surfaceColorKey surface $= Just key
  t <- createTextureFromSurface r surface
  freeSurface surface
  return $ SDLTexture t size

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
    
destroyTextures :: Textures -> IO ()
destroyTextures ts = do
  destroyTexture $ getSDLTexture $ _humanM ts
  destroyTexture $ getSDLTexture $ _humanF ts
  destroyTexture $ getSDLTexture $ _soil ts
  destroyTexture $ getSDLTexture $ _grass ts
  destroyTexture $ getSDLTexture $ _stones ts

    
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

-- drawGrid :: (MonadIO m) => Renderer -> m ()
-- drawGrid renderer = do
  
--   rendererRenderTarget renderer $= t 

-- void gridDraw() {
--   // Render all tiles
--   if(isEntireGridIsDrawn) return;
  
--   SDL_SetRenderTarget(renderer, gridTexture);
--   printf("gridRender%d, %d\n", GRID_WIDTH, GRID_HEIGHT);
--   for(int i = 0; i < GRID_WIDTH; ++i) {
--       for(int j = 0; j < GRID_HEIGHT; ++j) {
--           gridDrawTile(&(grid->tiles[i][j]));
--       }
--   }
--   SDL_SetRenderTarget(renderer, NULL);
--   isEntireGridIsDrawn = true;
-- }
  
-- void gridDrawTile(Tile *tile) {
--     RectAndTexture* RectAndTexture = &tile->RectAndTexture;
--     SDL_RenderCopy(renderer, RectAndTexture->texture, NULL, &RectAndTexture->rect);
-- }    

