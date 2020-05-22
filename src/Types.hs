{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Types where

import           Data.Label
import           Foreign.C.Types
import           GHC.Word        (Word32)
import           Reflex.SDL2

------- Graphics -------------
data SDLTexture = SDLTexture {
  _filePath      :: !FilePath,
  _getSDLTexture :: !Texture,
  _sizeT         :: !(V2 CInt)
} deriving (Eq)

instance Show SDLTexture where
    show (SDLTexture n _ _) = "SDLTexture " ++ n

data Textures = Textures {
  _humanM :: !SDLTexture,
  _humanF :: !SDLTexture,
  _soil   :: !SDLTexture,
  _grass  :: !SDLTexture,
  _stone  :: !SDLTexture,
  _water  :: !SDLTexture
} deriving (Eq, Show)

------- Input -----------------
data Input = Input {
  _currentFPS :: !Int,
  _mouseInput :: !(MouseButtonEventData, MouseMotionEventData)
} deriving (Eq, Show)

------- Time ------------------
data Time = Time {
  _elapsed     :: !Word32,
  _frameLimit  :: !Word32,
  _isGameFrame :: !Bool -- check if its time to change frame
} deriving (Eq, Show)

------- Game ------------------
data GameState = GameState {
  _camera      :: !(Point V2 CInt),
  _player      :: !(GameObject),
  _gameObjects :: ![GameObject]
} deriving (Eq, Show)

data GameObject = GameObject {
  _id             :: !Int,
  _speed          :: !CFloat,
  _position       :: !(Point V2 CFloat),
  _texture        :: !SDLTexture,
  _gameObjectType :: !GameObjectType
} deriving (Eq, Show)

data GameObjectType = Player
                    | AnimalTag Animal
                    | Plant
                    | Water
                    | Collectable
                    deriving (Show)

instance Eq GameObjectType where
    Player == Player  = True
    AnimalTag _ == AnimalTag _ = True
    _ == _ = False

data Animal = Animal {
  _gender        :: !Gender,
  _age           :: !CFloat, -- for timing actions
  _hp            :: !CFloat,
  _hydration     :: !CFloat,
  _nutrition     :: !CFloat,
  _sight         :: !CFloat,
  _MAX_HP        :: !CFloat,
  _MAX_HYDRATION :: !CFloat,
  _MAX_NUTRITION :: !CFloat
} deriving (Eq, Show)

data Gender = Male
            | Female { _numberOfFetuses      :: !CFloat,
                       _pregnancyStartTime   :: !CFloat,
                       _MAX_MULTIPLE_FETUSES :: !CFloat,
                       _PREGNANCY_TIME       :: !CFloat }
            deriving (Eq, Show)

initialHumanMale :: GameObjectType
initialHumanMale = AnimalTag Animal { _gender = Male,
                            _age = 0,
                            _hp = 100,
                            _hydration = 100,
                            _nutrition = 100,
                            _sight = 300,
                            _MAX_HP = 100,
                            _MAX_HYDRATION= 100,
                            _MAX_NUTRITION = 100 }

initialHumanFemale :: GameObjectType
initialHumanFemale = AnimalTag Animal { _gender = Female { _numberOfFetuses = 0,
                                                 _pregnancyStartTime = 0,
                                                 _MAX_MULTIPLE_FETUSES = 2,
                                                 _PREGNANCY_TIME = 10},
                              _age = 0,
                              _hp = 100,
                              _hydration = 100,
                              _nutrition = 100,
                              _sight = 300,
                              _MAX_HP = 100,
                              _MAX_HYDRATION= 100,
                              _MAX_NUTRITION = 100 }

data RangeCFloat = RangeCFloat { _start :: CFloat,
                                 _size  :: CFloat
                               } deriving (Eq, Show)

mkLabels [''GameState, ''GameObject, ''Animal, ''Gender, ''Time, ''Input]
