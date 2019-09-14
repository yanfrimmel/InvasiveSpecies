{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Game
import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Reflex
import Reflex.SDL2
import Graphics

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
  :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader Renderer m)
  => m ()
guest = do
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."

  r <- ask
  evDelay <- getAsyncEvent $ threadDelay 3000000
  dDelay  <- holdDyn False $ True <$ evDelay
  commitLayers $ ffor dDelay $ \case
    False -> pure $ do
      clear r
      rendererDrawColor r $= V4 0 0 255 255
      present r
      liftIO $ putStrLn "False"
    True  -> pure $ do
      clear r
      rendererDrawColor r $= V4 0 0 0 255
      present r  
      liftIO $ putStrLn "True"

  -- let performDeltaSecondTimer n = do
  --       evDelta  <- performEventDelta =<< tickLossyFromPostBuildTime n
  --       dTicks   <- foldDyn (+) 0 $ (1 :: Int) <$ evDelta
  --       dDelta   <- holdDyn 0 evDelta
  --       dElapsed <- foldDyn (+) 0 evDelta
  --       flip putDebugLnE id $ updated $ do
  --         tickz <- dTicks
  --         lapse <- dElapsed
  --         delta <- dDelta
  --         return $ unwords [ show n
  --                          , "timer -"
  --                          , show tickz
  --                          , "ticks -"
  --                          , show lapse
  --                          , "lapsed -"
  --                          , show delta
  --                          , "delta since last tick"
  --                          ]
  -- performDeltaSecondTimer 1
  -- evMouseMove <- getMouseMotionEvent
  -- dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  -- commitLayer $ ffor dMoves $ \moves ->
  --   forM_ (reverse moves) $ \dat -> do
  --     let P pos = fromIntegral <$> mouseMotionEventPos dat
  --         color = if null (mouseMotionEventState dat)
  --                 then V4 255 255 0   128
  --                 else V4 0   255 255 128
  --     renderAABB r color pos    

app :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
app = do
  (_, dynLayers) <- runDynamicWriterT guest
  r <- ask
  performEvent_ $ ffor (updated dynLayers) $ \layers -> do
    rendererDrawColor r $= V4 128 0 0 255
    clear r
    sequence_ layers
    present r     
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit

main :: IO ()
main = do
 withSDL $ withSDLImage $
  withWindow "InvasiveSpecies" (800, 600) $ \world -> 
   withRenderer world $ \renderer ->
    host $ runReaderT app renderer
