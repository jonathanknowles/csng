module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , folds
    , permutations
    , splitLast
    , splitWhen
    , zip3
    , unzip3
    ) where

import Data.Foldable
    ( foldrM )
import Data.List.NonEmpty

import Prelude hiding
    ( zip3, unzip3 )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

folds :: Semigroup a => NonEmpty a -> NonEmpty (NonEmpty a)
folds ws = case splitLast ws of
    Just (prefix, lastElement) ->
        foldrM acc (NE.singleton lastElement) prefix
    Nothing ->
        NE.singleton ws
  where
    acc a (x :| xs) = (a <> x :| xs) :| [a :| x : xs]

permutations :: NonEmpty a -> NonEmpty (NonEmpty a)
permutations as = NE.fromList <$> NE.fromList (L.permutations (NE.toList as))

splitLast :: NonEmpty a -> Maybe (NonEmpty a, a)
splitLast ps =
    case NE.reverse ps of
        (p :| q : rs) ->
            Just (NE.reverse (q :| rs), p)
        _ ->
            Nothing

splitWhen :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
splitWhen f (a :| as) =
    go (a :| []) as
  where
    go (p :| ps) (q : qs) | not (f p q) = go (q <| p :| ps) qs
    go ps qs = (NE.reverse ps, qs)

zip3 :: NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty (a, b, c)
zip3 (x :| xs) (y :| ys) (z :| zs) = (x, y, z) :| L.zip3 xs ys zs

unzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 xs =
    ( (\(a, _, _) -> a) <$> xs
    , (\(_, b, _) -> b) <$> xs
    , (\(_, _, c) -> c) <$> xs
    )
