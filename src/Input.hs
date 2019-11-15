module Input where

import Foreign.C.Types
import GHC.Word(Word32)
import Reflex
import Reflex.SDL2

data Inputs = Inputs {
  _currentFPS :: Integer,
  _mouseButtonEventData :: MouseButtonEventData -- todo add more inputs - such as AI or mouse events
} deriving (Eq, Show)
        
getMousePosition :: MouseButtonEventData -> Point V2 CInt
getMousePosition dat = pos
  where P pos32 = mouseButtonEventPos dat
        pos = P (fromIntegral <$> pos32)

isLeftButtonIsDown :: MouseButtonEventData -> Bool
isLeftButtonIsDown m = 
    mouseButtonEventMotion m == Pressed && mouseButtonEventButton m == ButtonLeft

isLeftButtonIsUp :: MouseButtonEventData -> Bool
isLeftButtonIsUp m = 
    mouseButtonEventMotion m == Released && mouseButtonEventButton m == ButtonLeft    
