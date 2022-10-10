module Data.Foldable.Extended
    ( module Data.Foldable
    , consecutivePairs
    , orderedPairs
    ) where

import Data.Foldable

import Safe
    ( tailMay )

import qualified Data.Foldable as F

-- | Returns all consecutive pairs from a structure.
--
-- Example:
--
-- >>> consecutivePairs [1 .. 4]
-- [(1, 2), (2, 3), (3, 4)]
--
consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs = inner . F.toList
  where
    inner xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys

-- | Returns all ordered pairs from a structure.
--
-- Example:
--
-- >>> orderedPairs [1 .. 4]
-- [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]
--
orderedPairs :: Foldable f => f a -> [(a, a)]
orderedPairs = inner . F.toList
  where
    inner [      ] = []
    inner [_     ] = []
    inner (x : xs) = [(x, y) | y <- xs] <> inner xs
