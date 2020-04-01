module Control.Process.Extra where

import Prelude
import Control.Monad.Iter.Trans (IterT, delay, runIterT)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Extra (now)
import Data.Time.Duration (Milliseconds, fromDuration)
import Effect (Effect)

sleep :: Milliseconds -> Effect Unit
sleep ms = runReaderT (runIterT go) =<< now
  where
  go :: IterT (ReaderT DateTime Effect) Unit
  go = do
    n <- lift $ lift $ now
    from <- lift $ ask
    if fromDuration (n `diff` from :: Milliseconds) > ms then
      pure unit
    else
      delay go
