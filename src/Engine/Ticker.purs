module Engine.Ticker where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Iter.Trans (IterT, delay)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, modify_)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Process.Extra (sleep)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Extra (now)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..), fromDuration)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

data TickerState
  = TickerState { fps :: Int, startTime :: DateTime, lastTickedTime :: DateTime }

initTickerState :: Effect TickerState
initTickerState = do
  t <- now
  pure $ TickerState { fps: 16, startTime: t, lastTickedTime: t }

newtype TickerT m a
  = Ticker (IterT (StateT TickerState m) a)

runTickerT :: forall m. Monad m => MonadRec m => MonadEffect m => TickerT m ~> m
runTickerT m = do
  s <- liftEffect initTickerState
  evalStateT (runFreeT go (unwrap m)) s
  where
  go :: forall a. Identity (IterT (StateT TickerState m) a) -> (StateT TickerState m) (IterT (StateT TickerState m) a)
  go (Identity a) = do
    now' <- liftEffect now
    TickerState { fps, lastTickedTime } <- get
    liftEffect $ sleep $ Milliseconds $ 1000.0 / toNumber fps
    modify_ \(TickerState s) -> TickerState (s { lastTickedTime = now' })
    pure a

derive instance newtypeTicker :: Newtype (TickerT m a) _

derive newtype instance functorTicker :: Functor m => Functor (TickerT m)

derive newtype instance applyTicker :: Monad m => Apply (TickerT m)

derive newtype instance applicativeTicker :: Monad m => Applicative (TickerT m)

derive newtype instance bindTicker :: Monad m => Bind (TickerT m)

derive newtype instance monadTicker :: Monad m => Monad (TickerT m)

derive newtype instance monadRecTicker :: MonadRec m => MonadRec (TickerT m)

derive newtype instance monadEffectTicker :: MonadEffect m => MonadEffect (TickerT m)

derive newtype instance monadStateTicker :: Monad m => MonadState TickerState (TickerT m)

instance monadTransTickerT :: MonadTrans TickerT where
  lift = wrap <<< lift <<< lift

tick :: forall m. Monad m => TickerT m Unit
tick = wrap $ delay (pure unit)

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
  TickerState { startTime } <- get
  pure $ fromDuration (n `diff` startTime :: Milliseconds)

getLastTickedTime :: forall m. Monad m => TickerT m DateTime
getLastTickedTime = do
  TickerState { lastTickedTime } <- get
  pure lastTickedTime

getStartTime :: forall m. Monad m => TickerT m DateTime
getStartTime = do
  TickerState { startTime } <- get
  pure startTime
