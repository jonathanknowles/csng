module Data.Traversable.Extended
    ( module Data.Traversable
    , fill
    , fillMaybe
    , mapTraverseList
    , mapTraverseListMaybe
    )
    where

import Data.Traversable

import qualified Data.Foldable as F

fill :: (Monoid b, Foldable f, Traversable t) => f b -> t a -> t b
fill xs = snd . mapAccumL fillInner (F.toList xs)
  where
    fillInner [] _ = ([], mempty)
    fillInner (y : ys) _ = (ys, y)

fillMaybe :: (Foldable f, Traversable t) => f b -> t a -> Maybe (t b)
fillMaybe xs = sequenceA . snd . mapAccumL fillInner (F.toList xs)
  where
    fillInner [] _ = ([], Nothing)
    fillInner (y : ys) _ = (ys, Just y)

mapTraverseList :: (Monoid b, Traversable t) => ([a] -> [b]) -> t a -> t b
mapTraverseList f as = fill (f (F.toList as)) as

mapTraverseListMaybe :: (Traversable t) => ([a] -> [b]) -> t a -> Maybe (t b)
mapTraverseListMaybe f as = fillMaybe (f (F.toList as)) as
