module Time where

import           GHC.Word (Word32)
import           Types

second :: Float
second  = 1000

createTime :: Word32 -> Time
createTime limit = Time 0 limit True

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (fLimit, delta) time =
  time
    { _elapsed     = newElapsed
    , _lastFrame   = fst newFrame
    , _isGameFrame = snd newFrame
    }
    where newElapsed = _elapsed time + delta
          limit
            | fLimit == 0 = 0
            | otherwise = second / fromIntegral (fLimit)
          newFrame
            | toRational newElapsed - toRational (_lastFrame time) > toRational limit = (newElapsed, True) 
            | otherwise                                                               = ((_lastFrame time), False)
