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
import Graphics
import Control.Concurrent   (threadDelay)
import Control.Monad        (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Reflex
import Reflex.SDL2
import Foreign.C.Types
import GHC.Word(Word32)

main :: IO ()
main = do
 withSDL $ withSDLImage $ withSDLFont $
  withWindow "InvasiveSpecies" $ \world -> 
   withRenderer world $ \renderer ->
    withTextures renderer $ \(renderer, textures) ->
      host $ runReaderT app $ (renderer, textures)
