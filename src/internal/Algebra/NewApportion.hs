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
import Data.Semigroup
    ( sconcat )
import Data.Map.Strict
    ( Map )
import Numeric.Natural
    ( Natural )

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Semigroup a) => Apportion a where

    type Weight a

    apportion :: a -> NonEmpty (Weight a) -> (a, NonEmpty a)

    default apportion
        :: Monoid a => a -> NonEmpty (Weight a) -> (a, NonEmpty a)
    apportion a as = case apportionMaybe a as of
        Nothing -> (a, mempty <$ as)
        Just bs -> (mempty, bs)

    apportionMaybe :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)

    default apportionMaybe
        :: (Eq a, Monoid a) => a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       (b, bs) | b == mempty -> Just bs
       (_, _) -> Nothing

apportionLawLength :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLawLength a ws =
    apportion a ws & \(_, rs) -> length rs == length ws

apportionLawSum :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLawSum a ws =
    apportion a ws & \(r, rs) -> sconcat (NE.cons r rs) == a

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class Apportion a => BalancedApportion a where

    type Exact a

    apportionDeviation :: a -> Exact a -> Ratio Natural
    apportionExact :: a -> NonEmpty (Weight a) -> (Exact a, NonEmpty (Exact a))
    apportionOrder :: a -> a -> Bool

balancedApportionLawDeviation
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLawDeviation a ws =
    all (<= (1 % 1)) $ NE.zipWith apportionDeviation
        (uncurry NE.cons (apportion      a ws))
        (uncurry NE.cons (apportionExact a ws))

balancedApportionLawExactLength
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLawExactLength a ws =
    apportionExact a ws & \(_, rs) -> length rs == length ws

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
    apportionOrder = undefined

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
    apportionOrder = undefined

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
    apportionOrder = undefined

--------------------------------------------------------------------------------
-- Instances: Map Keys
--------------------------------------------------------------------------------

newtype Keys a = Keys
    { unKeys :: a }
    deriving (Eq, Monoid, Semigroup, Show)

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
    apportionOrder = undefined

--------------------------------------------------------------------------------
-- Instances: Map Values
--------------------------------------------------------------------------------

newtype Values a = Values
    { unValues :: a }
    deriving (Eq, Monoid, Semigroup, Show)

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
    apportionOrder = undefined

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
