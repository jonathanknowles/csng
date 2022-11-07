{- HLINT ignore "Use drop" -}
{- HLINT ignore "Use take" -}
{- HLINT ignore "Use splitAt" -}

module Data.Sliceable
    ( Sliceable (..)
    , takeMany
    ) where

import Data.IntCast
    ( intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Data.MonoidMap
    ( MonoidMap )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Traversable
    ( mapAccumL )
import Data.Tuple
    ( swap )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( drop, splitAt, take )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Sliceable a where

    type SliceableSize a

    drop :: SliceableSize a -> a -> a
    default drop :: SliceableSize a -> a -> a
    drop s a = snd (splitAt s a)

    take :: SliceableSize a -> a -> a
    default take :: SliceableSize a -> a -> a
    take s a = fst (splitAt s a)

    splitAt :: SliceableSize a -> a -> (a, a)
    default splitAt :: SliceableSize a -> a -> (a, a)
    splitAt s a = (take s a, drop s a)

    size :: a -> SliceableSize a

    {-# MINIMAL (drop, take, size) | (splitAt, size) #-}

instance Sliceable [a] where
    type SliceableSize [a] = Natural
    size = foldableSize
    splitAt = L.splitAt . naturalToInt

instance Sliceable (Map k v) where
    type SliceableSize (Map k v) = Natural
    size = foldableSize
    splitAt = Map.splitAt . naturalToInt

instance Sliceable (MonoidMap k v) where
    type SliceableSize (MonoidMap k v) = Natural
    size = foldableSize
    splitAt = MonoidMap.splitAt . naturalToInt

instance Sliceable (Set a) where
    type SliceableSize (Set a) = Natural
    size = foldableSize
    splitAt = Set.splitAt . naturalToInt

takeMany :: (Traversable t, Sliceable a) => t (SliceableSize a) -> a -> t a
takeMany ss a0 = snd $ mapAccumL takeOne a0 ss
  where
    takeOne a s = swap $ splitAt s a

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

foldableSize :: Foldable f => f a -> Natural
foldableSize = fromIntegral @Int @Natural . F.length

naturalToInt :: Natural -> Int
naturalToInt = fromMaybe maxBound . intCastMaybe @Natural @Int
