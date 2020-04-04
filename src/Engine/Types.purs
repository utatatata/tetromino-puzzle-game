module Engine.Types where

import Prelude
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy)
import Data.List.NonEmpty.Extra (minimumBy)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Tuple (fst)

data Window
  = FullScreen
  | Window Size

data Axis
  = Lattice Vector3
  | Midpoint Vector3

type FPS
  = Int

type Size
  = { width :: Int, height :: Int }

type Vector2
  = { x :: Int, y :: Int }

type Vector3
  = { x :: Int, y :: Int, z :: Int }

projectOnXY :: Vector3 -> Vector2
projectOnXY { x, y, z } = { x, y }

type Plane a
  = Map Vector2 a

type Space a
  = Map Vector3 a

projectOnScreen :: forall a. Space a -> Plane a
projectOnScreen =
  fromFoldable
    <<< map (bimap projectOnXY identity <<< minimumBy (compare `on` fst))
    <<< groupBy (eq `on` (projectOnXY <<< fst))
    <<< toUnfoldable
