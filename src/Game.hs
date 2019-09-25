{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE BlockArguments        #-}

module Game where

import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Reflex
import Reflex.SDL2
import Graphics
import Foreign.C.Types
import GHC.Word(Word32)
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

game
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader (Renderer, Textures) m)
  => m ()
game = do
  --TODO: understand how to create game loop--------------------
  gameReady <- getPostBuild
  (r, t) <- ask
  
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
  performEvent_ $ fmap (const fpsPrint fps) (updated fps)
  
  -- render grid every frame  
  let textureToPostions :: SDLTexture -> [Point V2 CInt] = \t -> [P (V2 0 0)] 
  commitLayer $ ffor deltaCount $ \a -> 
    renderGrid r t textureToPostions

  -- Show FPS on screen sceen once a second
  rf <- regularFont 
  commitLayer $ ffor fps $ \a -> 
    renderSolidText r rf (V4 255 255 0 255) ("FPS: " ++ show a) 0 0
  ----------------------------------
  
app :: (ReflexSDL2 t m, MonadReader (Renderer, Textures) m) => m ()
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

-- Function to just print something to the screen
fpsPrint :: MonadIO m => Integer -> m ()
fpsPrint fps = liftIO $ putStrLn $ "FPS: " ++ show fps