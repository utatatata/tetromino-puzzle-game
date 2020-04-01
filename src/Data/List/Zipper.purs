module Data.List.Zipper where

import Prelude
import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.List (List(..), (:), drop)
import Data.List.Extra (iterate)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)

class LeftRight a where
  left :: a -> Maybe a
  right :: a -> Maybe a

iterateLeft :: forall lr. LeftRight lr => lr -> List lr
iterateLeft = iterate left

iterateRight :: forall lr. LeftRight lr => lr -> List lr
iterateRight = iterate right

data Zipper a
  = Zipper (List a) a (List a)

instance leftRightZipper :: LeftRight (Zipper a) where
  left (Zipper Nil _ _) = Nothing
  left (Zipper (l : ls) c rs) = Just $ Zipper ls l (c : rs)
  right (Zipper _ _ Nil) = Nothing
  right (Zipper ls c (r : rs)) = Just $ Zipper (c : ls) r rs

instance functorZipper :: Functor Zipper where
  map f (Zipper ls c rs) = Zipper (map f ls) (f c) (map f rs)

instance extendZipper :: Extend Zipper where
  extend f z = Zipper (map f $ drop 1 $ iterateLeft z) (f z) (map f $ drop 1 $ iterateRight z)

instance comonadZipper :: Comonad Zipper where
  extract (Zipper _ c _) = c

instance foldableZipper :: Foldable Zipper where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (Zipper ls c rs) = foldMap f ls <> f c <> foldMap f rs

instance traversableZipper :: Traversable Zipper where
  traverse f (Zipper ls c rs) = Zipper <$> traverse f ls <*> f c <*> traverse f rs
  sequence = traverse identity

newtype DoubleZipper a
  = DoubleZipper (Zipper (Zipper a))

up :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
up (DoubleZipper zz) = DoubleZipper <$> left zz

down :: forall a. DoubleZipper a -> Maybe (DoubleZipper a)
down (DoubleZipper zz) = DoubleZipper <$> right zz

iterateUp :: forall a. DoubleZipper a -> List (DoubleZipper a)
iterateUp = iterate up

iterateDown :: forall a. DoubleZipper a -> List (DoubleZipper a)
iterateDown = iterate down

innerLeft :: forall a. Zipper (Zipper a) -> Maybe (Zipper (Zipper a))
innerLeft (Zipper lzs cz rzs) = Zipper <$> traverse left lzs <*> left cz <*> traverse left rzs

innerRight :: forall a. Zipper (Zipper a) -> Maybe (Zipper (Zipper a))
innerRight (Zipper lzs cz rzs) = Zipper <$> traverse right lzs <*> right cz <*> traverse right rzs

iterateInnerLeft :: forall a. Zipper (Zipper a) -> List (Zipper (Zipper a))
iterateInnerLeft = iterate innerLeft

iterateInnerRight :: forall a. Zipper (Zipper a) -> List (Zipper (Zipper a))
iterateInnerRight = iterate innerRight

instance leftRightDoubleZipper :: LeftRight (DoubleZipper a) where
  left (DoubleZipper zz) = DoubleZipper <$> innerLeft zz
  right (DoubleZipper zz) = DoubleZipper <$> innerRight zz

instance functorDoubleZipper :: Functor DoubleZipper where
  map f (DoubleZipper zz) = DoubleZipper $ map (map f) zz

roll :: forall a. Zipper (Zipper a) -> Zipper (Zipper (Zipper a))
roll zz = Zipper (drop 1 $ iterateInnerLeft zz) zz (drop 1 $ iterateInnerRight zz)

instance extendDoubleZipper :: Extend DoubleZipper where
  extend f (DoubleZipper zz) = map f $ map DoubleZipper <<< DoubleZipper <<< roll $ roll zz

instance comonadDoubleZipper :: Comonad DoubleZipper where
  extract (DoubleZipper (Zipper _ (Zipper _ c _) _)) = c
