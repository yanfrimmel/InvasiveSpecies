{-# LANGUAGE ScopedTypeVariables #-}

module Graphics (withSDL, withSDLImage, withSDLFont, withWindow,
withRenderer, withTextures, renderGrid, renderSolidText, windowWidth,
windowHeight, maxFrames, regularFont, checkIfTextureInFrame) where

import           Control.Monad          (forM_, void)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text
import           Foreign.C.Types
import           GHC.Word               (Word32)
import           Paths_InvasiveSpecies
import           Reflex.SDL2
import qualified SDL.Font
import qualified SDL.Image
import           Types

windowWidth :: CInt
windowWidth = 1000

windowHeight :: CInt
windowHeight = 1000

maxFrames :: Word32
maxFrames = 900

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
loadTexture r fileName = do
  path <- getDataFileName fileName
  putStrLn path
  (t, info) <- loadTextureWithInfo r path
  let size = V2 (textureWidth info) (textureHeight info)
  return $ SDLTexture path t size

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
  rendererDrawColor r $= backgroundColor
  -- rendererDrawBlendMode r $= BlendAlphaBlend
  void $ op r
  destroyRenderer r
  where backgroundColor = V4 130 72 38 255

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

renderGrid :: (MonadIO m) => Renderer -> [(SDLTexture, Point V2 CInt)] -> m ()
renderGrid r texturesAndPositions =
  forM_ texturesAndPositions (renderTextureInPosition r)

renderTextureInPosition :: (MonadIO m) => Renderer -> (SDLTexture , Point V2 CInt) -> m ()
renderTextureInPosition r (t,postion) =
  liftIO $ renderTexture r t postion

renderTexture :: Renderer -> SDLTexture -> Point V2 CInt -> IO ()
renderTexture r (SDLTexture _ t size) xy =
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
    t <- Reflex.SDL2.createTextureFromSurface r surface
    Reflex.SDL2.freeSurface surface
    size <- SDL.Font.size font text
    let (w, h) = size
        x' :: CInt = fromIntegral x
        y' :: CInt = fromIntegral y
    Reflex.SDL2.copy r t Nothing (Just (Rectangle (P $ V2 x' y') (V2 (fromIntegral w) (fromIntegral h))))
    Reflex.SDL2.destroyTexture t

 -- Load a font from a file
getFontFromFile :: MonadIO m => FilePath -> Int -> m SDL.Font.Font
getFontFromFile path size = do
  liftIO $ putStrLn ("Loading font: " ++ show path)
  SDL.Font.load path size

regularFont :: MonadIO m => m SDL.Font.Font
regularFont = do
  path <- liftIO $ getDataFileName "assets/ObliviousFont.ttf"
  getFontFromFile path 20

checkIfTextureInFrame :: Point V2 CInt -> (SDLTexture, Point V2 CInt) -> Bool
checkIfTextureInFrame (P (V2 x y)) (SDLTexture _ _ (V2 xt yt), P (V2 x2 y2))
  | x2 + xt < x                = False
  | y2 + yt < y                = False
  | x2 - xt > x + windowWidth  = False
  | y2 - yt > y + windowHeight = False
  | otherwise                  = True
