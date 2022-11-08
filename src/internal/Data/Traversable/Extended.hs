module Data.Traversable.Extended
    ( module Data.Traversable
    , fill
    , fillMaybe
    , mapTraverseList
    , mapTraverseListMaybe
    , mapTraverseNonEmpty
    , mapTraverseNonEmptyMaybe
    )
    where

import Data.Traversable

import Data.List.NonEmpty
    ( NonEmpty (..) )

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

mapTraverseList
    :: (Monoid b, Traversable t)
    => ([a] -> [b])
    -> t a
    -> t b
mapTraverseList f as = fill (f (F.toList as)) as

mapTraverseListMaybe
    :: (Traversable t)
    => ([a] -> [b])
    -> t a
    -> Maybe (t b)
mapTraverseListMaybe f as = fillMaybe (f (F.toList as)) as

mapTraverseNonEmpty
    :: (Monoid b, Traversable t)
    => (NonEmpty a -> NonEmpty b)
    -> t a
    -> t b
mapTraverseNonEmpty f as = case F.toList as of
    [] -> fill [] as
    (x : xs) -> fill (f (x :| xs)) as

mapTraverseNonEmptyMaybe
    :: (Monoid b, Traversable t)
    => (NonEmpty a -> NonEmpty b)
    -> t a
    -> Maybe (t b)
mapTraverseNonEmptyMaybe f as = case F.toList as of
    [] -> fillMaybe [] as
    (x : xs) -> fillMaybe (f (x :| xs)) as
