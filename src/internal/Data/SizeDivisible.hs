module Data.SizeDivisible
    ( SizeDivisible (..)
    , splitAtMany
    ) where

import Data.IntCast
    ( intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Strict.Map
    ( Map )
import Data.Strict.Set
    ( Set )
import Data.Sized
    ( Sized (..) )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( splitAt )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Strict.Map as Map
import qualified Data.Strict.Set as Set

class Sized a => SizeDivisible a where
    drop :: Size a -> a -> a
    take :: Size a -> a -> a
    splitAt :: Size a -> a -> (a, a)

instance SizeDivisible [a] where
    drop = L.drop . naturalToInt
    take = L.take . naturalToInt
    splitAt = L.splitAt . naturalToInt

instance SizeDivisible (Map k v) where
    drop = Map.drop . naturalToInt
    take = Map.take . naturalToInt
    splitAt = Map.splitAt . naturalToInt

instance SizeDivisible (Set a) where
    drop = Set.drop . naturalToInt
    take = Set.take . naturalToInt
    splitAt = Set.splitAt . naturalToInt

splitAtMany :: forall a. SizeDivisible a => NonEmpty (Size a) -> a -> NonEmpty a
splitAtMany chunkSizes a = NE.unfoldr makeChunk (chunkSizes, a)
  where
    makeChunk :: (NonEmpty (Size a), a) -> (a, Maybe (NonEmpty (Size a), a))
    makeChunk (l :| mls, r) = case NE.nonEmpty mls of
        Just ls -> (prefix, Just (ls, suffix))
        Nothing -> (r, Nothing)
      where
        (prefix, suffix) = splitAt l r

naturalToInt :: Natural -> Int
naturalToInt = fromMaybe maxBound . intCastMaybe @Natural @Int
