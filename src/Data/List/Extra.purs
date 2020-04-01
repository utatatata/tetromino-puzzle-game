module Data.List.Extra where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

iterate :: forall a. (a -> Maybe a) -> a -> List a
iterate f = flip go Nil
  where
  go a accum = case f a of
    Nothing -> a : accum
    Just a' -> go a' (a : Nil)
