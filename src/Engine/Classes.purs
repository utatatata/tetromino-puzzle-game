module Engine.Classes where

import Prelude
import Data.FoldableWithIndex (forWithIndex_)
import Data.String.CodeUnits as SCU
import Engine.Types (Axis, Vector3)

class
  Functor f <= Location f where
  getLocation :: f Vector3

class
  Functor f <= Affine f where
  translate :: Vector3 -> f ~> f
  rotateL :: Axis -> f ~> f
  rotateR :: Axis -> f ~> f

class
  (Monad m, Affine m, Location m) <= Drawable m where
  dot :: Vector3 -> Char -> m Unit

fromArray :: forall m. Drawable m => Array String -> m Unit
fromArray rows =
  forWithIndex_ rows \y row ->
    forWithIndex_ (SCU.toCharArray row) \x c ->
      dot { x: x, y: y, z: 0 } c
