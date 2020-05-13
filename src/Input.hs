module Input where

import           Foreign.C.Types
import           Reflex.SDL2

data Inputs = Inputs {
  _currentFPS :: !Integer,
  _mouseInput :: !(MouseButtonEventData, MouseMotionEventData) -- todo add more inputs - such as AI or mouse events
} deriving (Eq, Show)

getMouseButtonClickPosition :: MouseButtonEventData -> Point V2 CInt
getMouseButtonClickPosition dat = pos
  where P pos32 = mouseButtonEventPos dat
        pos = P (fromIntegral <$> pos32)

getMousePosition :: MouseMotionEventData -> Point V2 CInt
getMousePosition dat = pos
  where P pos32 = mouseMotionEventPos dat
        pos = P (fromIntegral <$> pos32)

isLeftButtonDown :: MouseButtonEventData -> Bool
isLeftButtonDown m =
    mouseButtonEventMotion m == Pressed && mouseButtonEventButton m == ButtonLeft

isLeftButtonUp :: MouseButtonEventData -> Bool
isLeftButtonUp m =
    mouseButtonEventMotion m == Released && mouseButtonEventButton m == ButtonLeft
