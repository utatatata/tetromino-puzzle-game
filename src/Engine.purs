module Engine
  ( GameConfig
  , defaultConfig
  , GameLoop
  , runGameLoop
  , module Exports
  ) where

import Prelude
import Effect (Effect)
import Engine.Classes (class Location, class Affine, class Drawable, dot, fromArray, getLocation, rotateL, rotateR, translate) as Exports
import Engine.CLI as CLI
import Engine.Drawing (Drawing) as Exports
import Engine.Frame (Frame, runFrame)
import Engine.Frame (Frame, draw, flush, keyStates, windowSize) as Exports
import Engine.Ticker (TickerT, runTickerT)
import Engine.Ticker (Ticker, TickerT, tick, getDeltaTime, getElapsedTime, getFPS, getLastTickedTime, getStartTime, setFPS) as Exports
import Engine.Types (FPS, Window(..))
import Engine.Types (Axis(..), FPS, Plane, Size, Space, Vector2, Vector3, Window(..), projectOnScreen, projectOnXY) as Exports

type GameConfig
  = { window :: Window, fps :: FPS }

defaultConfig :: GameConfig
defaultConfig = { window: FullScreen, fps: 16 }

type GameLoop
  = TickerT Frame

runGameLoop :: GameConfig -> GameLoop ~> Effect
runGameLoop { window, fps } m = do
  CLI.hideCursor
  CLI.resetCursor
  CLI.clearScreenDown (pure unit)
  runFrame window <<< (runTickerT fps) $ m
