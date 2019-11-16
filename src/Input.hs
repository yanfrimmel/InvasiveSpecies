module Input where

import Foreign.C.Types
import GHC.Word(Word32)
import Reflex
import Reflex.SDL2

data Inputs = Inputs {
  _currentFPS :: Integer,
  _mouseInput :: (MouseButtonEventData, MouseMotionEventData) -- todo add more inputs - such as AI or mouse events
} deriving (Eq, Show)
        
getMousePosition :: MouseButtonEventData -> Point V2 CInt
getMousePosition dat = pos
  where P pos32 = mouseButtonEventPos dat
        pos = P (fromIntegral <$> pos32)

isLeftButtonDown :: MouseButtonEventData -> Bool
isLeftButtonDown m = 
    mouseButtonEventMotion m == Pressed && mouseButtonEventButton m == ButtonLeft

isLeftButtonUp :: MouseButtonEventData -> Bool
isLeftButtonUp m = 
    mouseButtonEventMotion m == Released && mouseButtonEventButton m == ButtonLeft    
