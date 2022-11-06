{- HLINT ignore "Use drop" -}
{- HLINT ignore "Use take" -}
{- HLINT ignore "Use splitAt" -}

module Data.SizeDivisible
    ( SizeDivisible (..)
    , takeMany
    ) where

import Data.IntCast
    ( intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.MonoidMap
    ( MonoidMap )
import Data.Sized
    ( Sized (..) )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( drop, splitAt, take )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Sized a => SizeDivisible a where

    drop :: Size a -> a -> a
    default drop :: Size a -> a -> a
    drop s a = snd (splitAt s a)

    take :: Size a -> a -> a
    default take :: Size a -> a -> a
    take s a = fst (splitAt s a)

    splitAt :: Size a -> a -> (a, a)
    default splitAt :: Size a -> a -> (a, a)
    splitAt s a = (take s a, drop s a)

    {-# MINIMAL (drop, take) | splitAt #-}

instance SizeDivisible [a] where
    splitAt = L.splitAt . naturalToInt

instance SizeDivisible (Map k v) where
    splitAt = Map.splitAt . naturalToInt

instance SizeDivisible (MonoidMap k v) where
    splitAt = MonoidMap.splitAt . naturalToInt

instance SizeDivisible (Set a) where
    splitAt = Set.splitAt . naturalToInt

takeMany :: forall a. SizeDivisible a => [Size a] -> a -> [a]
takeMany chunkSizes a = case chunkSizes of
    [ ]    -> [ ]
    [_]    -> [a]
    s : ss -> toList $ NE.unfoldr makeChunk (s :| ss, a)
  where
    makeChunk :: (NonEmpty (Size a), a) -> (a, Maybe (NonEmpty (Size a), a))
    makeChunk (l :| mls, r) = case NE.nonEmpty mls of
        Just ls -> (prefix, Just (ls, suffix))
        Nothing -> (take l r, Nothing)
      where
        (prefix, suffix) = splitAt l r

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

naturalToInt :: Natural -> Int
naturalToInt = fromMaybe maxBound . intCastMaybe @Natural @Int
