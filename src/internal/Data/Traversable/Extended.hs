{- HLINT ignore "Functor law" -}

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
    :: forall t a b s. (Traversable t, Ord a)
    => (s -> a -> (s, b))
    -> s
    -> t a
    -> (s, t b)
mapAccumSortedL accum0 state0 elements
    = index
    & fmap snd
    & mapAccumL accum0 state0
    & fmap (fmap snd . L.sortOn fst . L.zip (fst <$> index))
    & fmap (`fillUnsafe` elements)
  where
    index :: [(Int, a)]
    index
        = elements
        & F.toList
        & L.zip (L.iterate (+ 1) 0)
        & L.sortOn snd

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
