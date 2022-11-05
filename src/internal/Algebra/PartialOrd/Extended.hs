{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.PartialOrd.Extended
    ( module Algebra.PartialOrd
    )
    where

import Algebra.PartialOrd

import Data.Monoid
    ( Sum )
import Data.MonoidMap
    ( MonoidMap )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap

instance PartialOrd Natural where
    leq = (<=)

instance PartialOrd (Ratio Natural) where
    leq = (<=)

instance PartialOrd (Sum Natural) where
    leq = (<=)

instance PartialOrd (Sum (Ratio Natural)) where
    leq = (<=)

instance (Ord k, PartialOrd v) => PartialOrd (MonoidMap k v) where
    leq a b = Map.isSubmapOfBy leq (MonoidMap.toMap a) (MonoidMap.toMap b)
