module Types where

import           Foreign.C.Types
import           Reflex.SDL2
import           GHC.Word (Word32)

------- Graphics -------------
data SDLTexture = SDLTexture {
  _getSDLTexture :: !Texture,
  _sizeT         :: !(V2 CInt)
} deriving (Eq)

data Textures = Textures {
  _humanM :: !SDLTexture,
  _humanF :: !SDLTexture,
  _soil   :: !SDLTexture,
  _grass  :: !SDLTexture,
  _stone  :: !SDLTexture,
  _water  :: !SDLTexture
} deriving (Eq)

------- Input -----------------
data Inputs = Inputs {
  _currentFPS :: !Int,
  _mouseInput :: !(MouseButtonEventData, MouseMotionEventData) -- todo add more inputs - such as AI or mouse events
} deriving (Eq, Show)

------- Time ------------------
data Time = Time {
  _elapsed    :: !Word32,
  _frameLimit :: !Word32,
  _isGameFrame  :: !Bool -- check if its time to change frame
} deriving (Eq, Show)

------- Game ------------------
data GameState = GameState {
  _camera      :: !(Point V2 CInt),
  _player      :: !GameObject,
  _gameObjects :: ![GameObject]
} deriving (Eq)

data GameObject = GameObject {
  _id       :: !Int,
  _speed    :: !CFloat,
  _position :: !(Point V2 CFloat),
  _texture  :: !SDLTexture
} deriving (Eq)





