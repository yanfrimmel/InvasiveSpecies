module Input where

import Foreign.C.Types
import GHC.Word(Word32)
import Reflex
import Reflex.SDL2

-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)

-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32

getMousePosition :: MouseButtonEventData -> V2 Int
getMousePosition dat = pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32      

isLeftButtonIsDown :: MouseButtonEventData -> Bool
isLeftButtonIsDown m = 
    mouseButtonEventMotion m == Pressed && mouseButtonEventButton m == ButtonLeft

-- ffor :: Functor f => f a -> (a -> b) -> f b
-- . (b -> c) -> (a -> b) -> a -> c
-- holdDyn :: MonadHold t m => a -> Event t a -> m (Dynamic t a)
-- getMouseDynamic :: Dynamic t MouseButtonEventData
-- getMouseDynamic = do
--     evMouseButton <- getMouseButtonEvent
--     dBtns         <- (holdDyn evMouseButton getMouseButtonEvent)
-- --     return dBtns
-- getMouseClickEvent :: Dynamic t MouseButtonEventData
-- getMouseClickEvent = do    
--     let defMouseButton = MouseButtonEventData Nothing Pressed (Mouse 0) ButtonLeft 0 (P $ V2 0 0) 
--     let mouseClickDyn = holdDyn defMouseButton =<< getMouseButtonEvent
--     (isLeftButtonIsDown) =<< mouseClickDyn
