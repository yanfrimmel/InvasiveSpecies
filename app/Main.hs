{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_InvasiveSpecies

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_InvasiveSpecies.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
        <$> switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                  )
    )
    empty
  logOptions <- logOptionsHandle stderr (optionsVerbose options)
  proceseContexts <- mkDefaultProcessContext
  withLogFunc logOptions $ \logFucntion ->
    let app = App
          { appLogFunc = logFucntion
          , appProcessContext = proceseContexts
          , appOptions = options
          }
      in runRIO app run