{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.PartialOrd.Extended
    ( module Algebra.PartialOrd
    , Infix (..)
    , Prefix (..)
    , Suffix (..)
    )
    where

import Algebra.PartialOrd

import Data.Monoid
    ( Sum )
import Data.MonoidMap
    ( MonoidMap )
import Data.Ratio
    ( Ratio )
import Data.Strict.Map
    ( Map )
import Numeric.Natural
    ( Natural )

import qualified Data.List as L
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Strict.Map as Map

instance PartialOrd Natural where
    leq = (<=)

instance PartialOrd (Ratio Natural) where
    leq = (<=)

instance PartialOrd (Sum Natural) where
    leq = (<=)

instance PartialOrd (Sum (Ratio Natural)) where
    leq = (<=)

instance (Ord k, PartialOrd v) => PartialOrd (Map k v) where
    leq = Map.isSubmapOfBy leq

instance (Ord k, PartialOrd v) => PartialOrd (MonoidMap k v) where
    leq a b = Map.isSubmapOfBy leq (MonoidMap.toMap a) (MonoidMap.toMap b)

--------------------------------------------------------------------------------
-- Infix
--------------------------------------------------------------------------------

newtype Infix a = Infix {getInfix :: a}
    deriving (Eq, Show)

instance Eq a => PartialOrd (Infix [a]) where
    Infix as `leq` Infix bs = as `L.isInfixOf` bs

--------------------------------------------------------------------------------
-- Prefix
--------------------------------------------------------------------------------

newtype Prefix a = Prefix {getPrefix :: a}
    deriving (Eq, Show)

instance Eq a => PartialOrd (Prefix [a]) where
    Prefix as `leq` Prefix bs = as `L.isPrefixOf` bs

--------------------------------------------------------------------------------
-- Suffix
--------------------------------------------------------------------------------

newtype Suffix a = Suffix {getSuffix :: a}
    deriving (Eq, Show)

instance Eq a => PartialOrd (Suffix [a]) where
    Suffix as `leq` Suffix bs = as `L.isSuffixOf` bs
