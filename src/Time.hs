module Time where

import           GHC.Word (Word32)
import           Types

second :: Float
second  = 1000

createTime :: Word32 -> Time
createTime limit = Time 0 limit True

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (fLimt, delta) time =
  time
    { _elapsed    = fst check
    , _frameLimit = fLimt
    , _isGameFrame  = snd  check
    }
    where newElapsed = _elapsed time + delta
          limit
            | _frameLimit time == 0 = 0
            | otherwise = second / fromIntegral (_frameLimit time)
          check
            | limit <= 0       = (delta, True) -- no limit
            | toRational newElapsed > toRational limit = (newElapsed, True)
            | otherwise        = (newElapsed, False) -- filter out this frame from the game
