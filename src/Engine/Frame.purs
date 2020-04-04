module Engine.Frame where

import Prelude
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.RWS (RWSResult(..), RWST, ask, get, modify_, put, runRWST)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Map as M
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Engine.Classes (class Drawable)
import Engine.CLI as CLI
import Engine.Drawing (runDrawing)
import Engine.Types (Size, Window, Space)

newtype Frame a
  = Frame (Free Command a)

runFrame :: Window -> Frame ~> Effect
runFrame win m = do
  RWSResult space a _ <- runRWST (foldFree runCommand (unwrap m)) win M.empty
  CLI.safeWrite win space
  pure a

data Command a
  = Draw (forall m. Drawable m => m a)
  | Flush a
  | KeyStates a
  | WindowSize (Size -> a)
  | LiftEffect (Effect a)

runCommand :: Command ~> RWST Window Unit (Space Char) Effect
runCommand (Draw m) = do
  Tuple a space <- lift $ runDrawing m
  modify_ (append space)
  pure a

runCommand (Flush a) = do
  win <- ask
  space <- get
  lift CLI.resetCursor
  lift $ CLI.safeWrite win space
  put M.empty
  pure a

runCommand (KeyStates a) = do
  pure a

runCommand (WindowSize a) = a <$> pure { width: 100, height: 50 }

runCommand (LiftEffect a) = lift a

derive instance newtypeFrame :: Newtype (Frame a) _

derive newtype instance functorFrame :: Functor Frame

derive newtype instance applyFrame :: Apply Frame

derive newtype instance applicativeFrame :: Applicative Frame

derive newtype instance bindFrame :: Bind Frame

derive newtype instance monadFrame :: Monad Frame

derive newtype instance monadRecFrame :: MonadRec Frame

instance monadEffectFrame :: MonadEffect Frame where
  liftEffect m = wrap $ liftF $ LiftEffect m

draw :: forall a. (forall m. Drawable m => m a) -> Frame a
draw m = wrap (liftF (Draw m))

flush :: Frame Unit
flush = wrap <<< liftF <<< Flush $ unit

keyStates :: Frame Unit
keyStates = wrap $ liftF $ KeyStates unit

windowSize :: Frame Size
windowSize = wrap $ liftF $ WindowSize identity
