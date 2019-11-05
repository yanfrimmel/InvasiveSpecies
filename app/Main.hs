{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Foreign.C.Types
import Game
import GHC.Word(Word32)
import Graphics
import Reflex
import Reflex.SDL2

main :: IO ()
main = do
 withSDL $ withSDLImage $ withSDLFont $
  withWindow "InvasiveSpecies" $ \world -> 
   withRenderer world $ \renderer ->
    withTextures renderer $ \(renderer, textures) ->
      host $ runReaderT app $ (renderer, textures)
