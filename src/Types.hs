{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant


data World = World
  { exiting :: Bool
  , panes   :: PaneMap
  }

data PaneMap = PaneMap
  { topLeft     :: Pane
  , topRight    :: Pane
  , bottomLeft  :: Pane
  , bottomRight :: Pane
  }

data Pane
  = Out
  | Over
  | Down
  | Up

data Quadrant
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight  
