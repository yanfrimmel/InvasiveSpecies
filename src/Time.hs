module Time where

import GHC.Word(Word32)

data Time = Time { 
  _elapsed :: !Word32,
  _frameLimit  :: !Word32,
  _nextFrame   :: !Bool,
  _postFrame   :: !Bool
} deriving (Eq, Show)

-- Easy way to create a Time
createTime :: Word32 -> Time
createTime limit = Time 0 limit True False

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (fLimt, delta) time =
  time
    { _elapsed = (\(a,_,_) -> a) check
    , _frameLimit = fLimt
    , _nextFrame = (\(_,a,_) -> a) check
    , _postFrame = (\(_,_,a)->a) check
    }
    where newAccum = _elapsed time + delta
          limit
            | _frameLimit time == 0 = 0
            | otherwise = round (1000 / fromIntegral (_frameLimit time))
          check
            | limit <= 0 = (delta, True, True) -- no limit 
            | _postFrame time = (mod newAccum limit, False, False)
            | newAccum > limit = (newAccum, True, True)
            | otherwise = (newAccum, False, False)    