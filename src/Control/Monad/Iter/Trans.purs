module Control.Monad.Iter.Trans where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Identity (Identity)
import Data.Newtype (unwrap)

type IterT
  = FreeT Identity

runIterT :: forall m. Monad m => MonadRec m => IterT m ~> m
runIterT = runFreeT (pure <<< unwrap)

-- delay = wrapFree <<< pure
-- but, so `MonadFree f (IterT m)` is a ophan instance, wrapFree can't be used.
delay :: forall m a. Monad m => IterT m a -> IterT m a
delay = join <<< liftFreeT <<< pure
