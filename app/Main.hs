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
import SDLUtils

app :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
app = do
 liftIO $ putStrLn "in app..."

main :: IO ()
main = do
 withSDL $ withSDLImage $
  withWindow "InvasiveSpecies" (800, 600) $ \world -> 
   withRenderer world $ \renderer ->
    host $ runReaderT app renderer
