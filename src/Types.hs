{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import           RIO
import           RIO.Process
import           Control.Lens

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
  logFuncL = RIO.lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL =
    RIO.lens appProcessContext (\x y -> x { appProcessContext = y })

data Intent
  = Idle
  | Quit
  | Press Quadrant
  | Release Quadrant
  | Hover Quadrant
  | Leave Quadrant


data World = World
  { exiting :: Bool
  , tiles   :: Tile
  }

data Tile = Tile
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

data UnitBase = UnitBase
  { _name :: String
  , _condition   :: Int
  , _position :: WorldPoint
  } deriving (Show)

data Animal = Animal
  { _unitBase :: UnitBase
  , _gender :: Gender
  , _diet :: Diet
  } deriving (Show)

data Gender
  = Male
  | Female
  deriving (Show)

data Diet
  = Carnivore
  | Omnivore
  | Herbivore
  deriving (Show)
  -- animal-vegetable-mineral  

data WorldPoint = WorldPoint
  { _xWorld :: Int
  , _yWorld :: Int
  } deriving (Show)

makeLenses ''UnitBase
makeLenses ''Animal
makeLenses ''Gender
makeLenses ''Diet
makeLenses ''WorldPoint
