{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module Algebra.NewApportion
    where

import Control.Arrow
    ( (&&&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Data.Semialign
    ( Semialign (..), Zip (..) )
import Data.Semigroup.Foldable
    ( Foldable1 (..) )
import Data.Strict.Map
    ( Map )
import Data.These
    ( These (..) )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( zip, zipWith )

import qualified Data.List.NonEmpty as NE
import qualified Data.Strict.Map as Map

--------------------------------------------------------------------------------
-- Apportionment
--------------------------------------------------------------------------------

data Apportionment a = Apportionment
    { leftover :: a
    , portions :: NonEmpty a
    }
    deriving stock (Eq, Foldable, Functor, Show)
    deriving anyclass Foldable1

instance Semialign Apportionment where
    align a0 a1 = Apportionment
        { leftover = These (leftover a0) (leftover a1)
        , portions = align (portions a0) (portions a1)
        }

instance Zip Apportionment where
    zip a0 a1 = Apportionment
        { leftover = (leftover a0, leftover a1)
        , portions = zip (portions a0) (portions a1)
        }

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Semigroup a) => Apportion a where

    type Weight a

    apportion :: a -> NonEmpty (Weight a) -> Apportionment a

    default apportion
        :: Monoid a => a -> NonEmpty (Weight a) -> Apportionment a
    apportion a as = case apportionMaybe a as of
        Nothing -> Apportionment a (mempty <$ as)
        Just bs -> Apportionment mempty bs

    apportionMaybe :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)

    default apportionMaybe
        :: (Eq a, Monoid a) => a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Apportionment b bs | b == mempty -> Just bs
       _ -> Nothing

apportionLawLength :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLawLength a ws =
    length (portions (apportion a ws)) == length ws

apportionLawSum :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLawSum a ws =
    fold1 (apportion a ws) == a

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class Apportion a => BalancedApportion a where

    type Exact a

    apportionDeviation :: a -> Exact a -> Ratio Natural
    apportionExact :: a -> NonEmpty (Weight a) -> Apportionment (Exact a)

balancedApportionLawDeviation
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLawDeviation a ws =
    all (<= (1 % 1)) $ zipWith apportionDeviation
        (apportion      a ws)
        (apportionExact a ws)

balancedApportionLawExactLength
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLawExactLength a ws =
    length (portions (apportionExact a ws)) == length ws

--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where
    type Weight (Sum Natural) = Natural
    apportion = undefined
    apportionMaybe = undefined

instance BalancedApportion (Sum Natural) where
    type Exact (Sum Natural) = Ratio Natural
    apportionExact = undefined
    apportionDeviation = undefined

--------------------------------------------------------------------------------
-- Instances: List
--------------------------------------------------------------------------------

instance Eq a => Apportion [a] where
    type Weight [a] = Natural
    apportion = undefined
    apportionMaybe = undefined

instance Eq a => BalancedApportion [a] where
    type Exact [a] = Ratio Natural
    apportionExact = undefined
    apportionDeviation :: [a] -> Ratio Natural -> Ratio Natural
    apportionDeviation as = distanceRatioNatural
        (naturalToRatio $ naturalLength as)

--------------------------------------------------------------------------------
-- Instances: Map
--------------------------------------------------------------------------------

instance (Ord k, Eq v) => Apportion (Map k v) where
    type Weight (Map k v) = Map k v
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v) => BalancedApportion (Map k v) where
    type Exact (Map k v) = Exact v
    apportionExact = undefined
    apportionDeviation = undefined

--------------------------------------------------------------------------------
-- Instances: Map Keys
--------------------------------------------------------------------------------

newtype Keys a = Keys
    { unKeys :: a }
    deriving newtype (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v) => Apportion (Keys (Map k v)) where
    type Weight (Keys (Map k v)) = Natural
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v) => BalancedApportion (Keys (Map k v)) where
    type Exact (Keys (Map k v)) = Ratio Natural
    apportionExact = undefined
    apportionDeviation :: Keys (Map k v) -> Ratio Natural -> Ratio Natural
    apportionDeviation (Keys m) = distanceRatioNatural
        (naturalToRatio $ naturalLength $ Map.keys m)

--------------------------------------------------------------------------------
-- Instances: Map Values
--------------------------------------------------------------------------------

newtype Values a = Values
    { unValues :: a }
    deriving newtype (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v, Apportion v) =>
    Apportion (Values (Map k v))
  where
    type Weight (Values (Map k v)) = Weight v
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v, BalancedApportion v) =>
    BalancedApportion (Values (Map k v))
  where
    type Exact (Values (Map k v)) = Exact v
    apportionExact = undefined
    apportionDeviation = undefined

--------------------------------------------------------------------------------
-- Apportioning with equal weights
--------------------------------------------------------------------------------

apportionEqual
    :: (BalancedApportion a, Integral (Weight a))
    => a
    -> NonEmpty void
    -> NonEmpty a
apportionEqual _a _ws = undefined -- snd (apportion a (1 <$ ws))

apportionEqualN
    :: (BalancedApportion a, Integral (Weight a))
    => a
    -> Int
    -> NonEmpty a
apportionEqualN a n =
    apportionEqual a (() :| replicate (max 0 (n - 1)) ())

bipartition :: (BalancedApportion a, Integral (Weight a)) => a -> (a, a)
bipartition = (NE.head &&& NE.last) . flip apportionEqual (() :| [()])

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

distanceRatioNatural :: Ratio Natural -> Ratio Natural -> Ratio Natural
distanceRatioNatural = undefined

naturalToRatio :: Natural -> Ratio Natural
naturalToRatio = fromIntegral

naturalLength :: Foldable f => f a -> Natural
naturalLength = fromIntegral @Int @Natural . length

apportionEqualMapKeys
    :: (Ord k, Eq v) => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapKeys m ws = unKeys <$> apportionEqual (Keys m) ws

apportionEqualMapValues
    :: (Ord k, Eq v, BalancedApportion v, Integral (Weight v))
    => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapValues m ws = unValues <$> apportionEqual (Values m) ws

apportionEqualNatural
    :: Sum Natural -> NonEmpty void -> NonEmpty (Sum Natural)
apportionEqualNatural = apportionEqual
