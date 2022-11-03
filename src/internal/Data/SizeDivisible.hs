module Data.SizeDivisible
    ( SizeDivisible (..)
    ) where

import Data.IntCast
    ( intCastMaybe )
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

import qualified Data.List as L
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

naturalToInt :: Natural -> Int
naturalToInt = fromMaybe maxBound . intCastMaybe @Natural @Int
