{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE BlockArguments        #-}

module Main where

import Game
import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Reflex
import Reflex.SDL2
import Graphics
import Foreign.C.Types

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

renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

guest
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m)
  => m ()
guest = do
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."
 
  -- evDelay <- getAsyncEvent $ threadDelay 3000000
  -- dDelay  <- holdDyn False $ True <$ evDelay
  -- commitLayers $ ffor dDelay $ \case
  --   False -> pure $ do
  --     clear r
  --     rendererDrawColor r $= V4 0 0 255 255
  --     present r
  --     liftIO $ putStrLn "False"
  --   True  -> pure $ do
  --     clear r
  --     rendererDrawColor r $= V4 0 0 0 255
  --     present r  
  --     liftIO $ putStrLn "True"


  
app :: (ReflexSDL2 t m, MonadReader (Renderer, Textures) m) => m ()
app = do
  -- (_, dynLayers) <- runDynamicWriterT guest
  (r, t) <- ask
  let textureToPostions :: SDLTexture -> [Point V2 CInt] = \t -> [P (V2 0 0)] 
  clear r
  renderGrid r t textureToPostions
  present r
  liftIO $ putStrLn "game end"
  
  -- performEvent_ $ ffor (updated dynLayers) $ \layers -> do
  --   rendererDrawColor r $= V4 128 0 0 255
  --   clear r
  --   sequence_ layers
  --   present r     
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit

main :: IO ()
main = do
 withSDL $ withSDLImage $
  withWindow "InvasiveSpecies" $ \world -> 
   withRenderer world $ \renderer ->
    withTextures renderer $ \(renderer, textures) ->
      host $ runReaderT app $ (renderer, textures)
