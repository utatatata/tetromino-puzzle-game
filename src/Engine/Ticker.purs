module Engine.Ticker where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Iter.Trans (IterT, delay)
import Control.Monad.RWS (RWST, ask, evalRWST)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, get, modify_)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell)
import Control.Process.Extra (sleep)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Extra (now)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..), fromDuration)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)

data TickerState
  = TickerState { fps :: Int, lastTickedTime :: DateTime }

data TickerReader
  = TickerReader { startTime :: DateTime }

newtype TickerT m a
  = Ticker (IterT (RWST TickerReader Unit TickerState m) a)

type TickerConfig
  = { fps :: Int }

runTickerT :: forall m. Monad m => MonadRec m => MonadEffect m => TickerConfig -> TickerT m ~> m
runTickerT { fps } m = do
  now' <- liftEffect now
  (Tuple a _) <- evalRWST (runFreeT go (unwrap m)) (TickerReader { startTime: now' }) (TickerState { fps: fps, lastTickedTime: now' })
  pure a
  where
  go :: forall a. Identity (IterT (RWST TickerReader Unit TickerState m) a) -> (RWST TickerReader Unit TickerState m) (IterT (RWST TickerReader Unit TickerState m) a)
  go (Identity a) = do
    now' <- liftEffect now
    TickerState { fps, lastTickedTime } <- get
    liftEffect $ sleep $ Milliseconds $ 1000.0 / toNumber fps
    modify_ \(TickerState s) -> TickerState (s { lastTickedTime = now' })
    pure a

derive instance newtypeTickerT :: Newtype (TickerT m a) _

derive newtype instance functorTickerT :: Functor m => Functor (TickerT m)

derive newtype instance applyTickerT :: Monad m => Apply (TickerT m)

derive newtype instance applicativeTickerT :: Monad m => Applicative (TickerT m)

derive newtype instance bindTickerT :: Monad m => Bind (TickerT m)

derive newtype instance monadTickerT :: Monad m => Monad (TickerT m)

derive newtype instance monadRecTickerT :: MonadRec m => MonadRec (TickerT m)

derive newtype instance monadEffectTickerT :: MonadEffect m => MonadEffect (TickerT m)

derive newtype instance moandAskTickerT :: Monad m => MonadAsk TickerReader (TickerT m)

derive newtype instance monadTell :: Monad m => MonadTell Unit (TickerT m)

derive newtype instance monadStateTickerT :: Monad m => MonadState TickerState (TickerT m)

instance monadTransTickerT :: MonadTrans TickerT where
  lift = wrap <<< lift <<< lift

tick :: forall m. Monad m => TickerT m Unit
tick = wrap <<< delay $ pure unit

setFPS :: forall m. Monad m => Int -> TickerT m Unit
setFPS fps = modify_ \(TickerState s) -> TickerState (s { fps = fps })

getFPS :: forall m. Monad m => TickerT m Int
getFPS = do
  TickerState { fps } <- get
  pure fps

getDeltaTime :: forall m. Monad m => MonadEffect m => TickerT m Milliseconds
getDeltaTime = do
  n <- liftEffect now
  TickerState { lastTickedTime } <- get
  pure $ fromDuration (n `diff` lastTickedTime :: Milliseconds)

getElapsedTime :: forall m. Monad m => MonadEffect m => TickerT m Milliseconds
getElapsedTime = do
  n <- liftEffect now
  TickerReader { startTime } <- ask
  pure $ fromDuration (n `diff` startTime :: Milliseconds)

getLastTickedTime :: forall m. Monad m => TickerT m DateTime
getLastTickedTime = do
  TickerState { lastTickedTime } <- get
  pure lastTickedTime

getStartTime :: forall m. Monad m => TickerT m DateTime
getStartTime = do
  TickerReader { startTime } <- ask
  pure startTime
