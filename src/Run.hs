{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO App ()
run = logInfo "We're inside the application!"
