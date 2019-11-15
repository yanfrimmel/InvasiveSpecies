{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Control.Monad.Reader (runReaderT)
import Game
import Graphics
import Reflex.SDL2

main :: IO ()
main = do
 withSDL $ withSDLImage $ withSDLFont $
  withWindow "InvasiveSpecies" $ \world -> 
   withRenderer world $ \renderer ->
    withTextures renderer $ \(rendererTextures, textures) ->
      host $ runReaderT app $ (rendererTextures, textures)
