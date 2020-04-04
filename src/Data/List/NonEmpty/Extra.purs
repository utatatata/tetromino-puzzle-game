module Data.List.NonEmpty.Extra where

import Prelude
import Data.Foldable as F
import Data.List.NonEmpty (NonEmptyList, head)
import Data.Maybe (maybe)

minimumBy :: forall a. (a -> a -> Ordering) -> NonEmptyList a -> a
minimumBy cmp xs = maybe (head xs) identity (F.minimumBy cmp xs)
