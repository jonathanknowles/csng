module Data.Traversable.Extended
    ( module Data.Traversable
    , fill
    , fillMaybe
    , mapAccumSortedL
    , mapTraverseList
    , mapTraverseListMaybe
    , mapTraverseNonEmpty
    , mapTraverseNonEmptyMaybe
    )
    where

import Data.Traversable

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )

import qualified Data.Foldable as F
import qualified Data.List as L

mapAccumSortedL
    :: forall state t a b. (Traversable t, Ord a)
    => (state -> a -> (state, b))
    -> state
    -> t a
    -> (state, t b)
mapAccumSortedL accum state as =
    remap <$> mapAccumL accum state (snd <$> index)
  where
    index :: [(Int, a)]
    index = L.sortOn snd $ L.zip (L.iterate (+ 1) 0) $ F.toList as

    remap :: [b] -> t b
    remap = (`fillUnsafe` as) . fmap snd . L.sortOn fst . L.zip (fst <$> index)

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

fillUnsafe :: (Traversable t, Foldable f) => f b -> t a -> t b
fillUnsafe as bs = fromMaybe bailOut (fillMaybe as bs)
  where
    bailOut = error "fillUnsafe"

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
