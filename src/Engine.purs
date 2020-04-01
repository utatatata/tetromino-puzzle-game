module Engine where

import Prelude
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Engine.Ticker (TickerT, runTickerT)

type Vector3
  = { x :: Int, y :: Int, z :: Int }

type Size
  = { width :: Int, height :: Int }

type GameConfig
  = { fps :: Int }

{-
defaultConfig :: GameConfig
defaultConfig = { fps: 16 }
-}
type GameLoop
  = TickerT Frame

runGameLoop :: GameLoop ~> Effect
runGameLoop = runFrame <<< runTickerT

newtype Frame a
  = Frame (Free Command a)

runFrame :: Frame ~> Effect
runFrame = foldFree runCommand <<< unwrap

data Command a
  = KeyStates a
  | WindowSize (Size -> a)
  | LiftEffect (Effect a)

runCommand :: Command ~> Effect
runCommand (KeyStates a) = do
  pure a

runCommand (WindowSize a) = a <$> pure { width: 100, height: 50 }

runCommand (LiftEffect a) = a

derive instance newtypeFrame :: Newtype (Frame a) _

derive newtype instance functorFrame :: Functor Frame

derive newtype instance applyFrame :: Apply Frame

derive newtype instance applicativeFrame :: Applicative Frame

derive newtype instance bindFrame :: Bind Frame

derive newtype instance monadFrame :: Monad Frame

derive newtype instance monadRecFrame :: MonadRec Frame

instance monadEffectFrame :: MonadEffect Frame where
  liftEffect m = wrap $ liftF $ LiftEffect m

keyStates :: Frame Unit
keyStates = wrap $ liftF $ KeyStates unit

windowSize :: Frame Size
windowSize = wrap $ liftF $ WindowSize identity
 {-
type Drawing
  = Free Draw

data Draw a
  = Dot Vector3 Char a

-}