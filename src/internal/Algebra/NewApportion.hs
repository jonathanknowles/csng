{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

import Data.List.NonEmpty.Extended
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Data.Semialign
    ( Semialign (..), Zip (..) )
import Data.Semigroup.Foldable
    ( Foldable1 (..) )
import Data.These
    ( These (..) )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( zip, zipWith )

import qualified Data.List.NonEmpty.Extended as NE

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
    alignWith f a0 a1 = Apportionment
        { leftover =           f $ These (leftover a0) (leftover a1)
        , portions = alignWith f         (portions a0) (portions a1)
        }

instance Zip Apportionment where
    zipWith f a0 a1 = Apportionment
        { leftover =         f (leftover a0) (leftover a1)
        , portions = zipWith f (portions a0) (portions a1)
        }

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Monoid a, Monoid (Weight a)) => Apportion a where

    type Weight a

    apportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default apportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    apportion a as = case apportionMaybe a as of
        Nothing -> Apportionment a (mempty <$ as)
        Just bs -> Apportionment mempty bs

    apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Apportionment b bs | b == mempty -> Just bs
       _ -> Nothing

apportionMaybe2
    :: Apportion a
    => a
    -> (Weight a, Weight a)
    -> Maybe (a, a)
apportionMaybe2 a (w1, w2) =
    apportionMaybe a [w1, w2] >>= \case
        [a1, a2] -> Just (a1, a2)
        _ -> Nothing

apportionMaybe3
    :: Apportion a
    => a
    -> (Weight a, Weight a, Weight a)
    -> Maybe (a, a, a)
apportionMaybe3 a (w1, w2, w3) =
    apportionMaybe a [w1, w2, w3] >>= \case
        [a1, a2, a3] -> Just (a1, a2, a3)
        _ -> Nothing

apportionLaw_leftover :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_leftover a ws =
    isJust (apportionMaybe a ws) == (fold1 (portions (apportion a ws)) == a)

apportionLaw_length :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_length a ws =
    length (portions (apportion a ws)) == length ws

apportionLaw_sum :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_sum a ws =
    fold1 (apportion a ws) == a

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class (Apportion a, ExactBalancedApportion (Exact a)) => BalancedApportion a
  where
    type Exact a

    balancedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default balancedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    balancedApportion = apportion

    balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    balancedApportionMaybe = apportionMaybe

balancedApportionLaw_identity
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity a ws =
    balancedApportion a ws == apportion a ws

balancedApportionLaw_identity_maybe
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity_maybe a ws =
    balancedApportionMaybe a ws == apportionMaybe a ws

--------------------------------------------------------------------------------
-- ExactBalancedApportion
--------------------------------------------------------------------------------

class BalancedApportion a => ExactBalancedApportion a where

    exactBalancedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    default exactBalancedApportion
        :: a -> NonEmpty (Weight a) -> Apportionment a
    exactBalancedApportion = apportion

    exactBalancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default exactBalancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    exactBalancedApportionMaybe = apportionMaybe

exactBalancedApportionLaw_identity
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_identity a ws =
    exactBalancedApportion a ws == apportion a ws

exactBalancedApportionLaw_identity_maybe
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_identity_maybe a ws =
    exactBalancedApportionMaybe a ws == apportionMaybe a ws

exactBalancedApportionLaw_folds
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_folds a ws =
    folds (portions (apportion a ws)) == (portions . apportion a <$> folds ws)

exactBalancedApportionLaw_splits
    :: ExactBalancedApportion a => a -> NonEmpty (Weight a) -> Bool
exactBalancedApportionLaw_splits a ws =
    traverse (fmap mid . apportionMaybe3 a) (splits ws) == apportionMaybe a ws

--------------------------------------------------------------------------------
-- Instances: Sum (Ratio Natural)
--------------------------------------------------------------------------------

instance Apportion (Sum (Ratio Natural)) where

    type Weight (Sum (Ratio Natural)) = Sum (Ratio Natural)

    apportionMaybe a ws
        | weightSum == mempty = Nothing
        | otherwise = Just (mkPortion <$> ws)
      where
        weightSum = fold1 ws
        mkPortion w = Sum (getSum a * getSum w / getSum weightSum)

instance BalancedApportion (Sum (Ratio Natural)) where

    type Exact (Sum (Ratio Natural)) = Sum (Ratio Natural)

instance ExactBalancedApportion (Sum (Ratio Natural))

--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where

    type Weight (Sum Natural) = Sum Natural

    apportion = undefined
    apportionMaybe = undefined

instance BalancedApportion (Sum Natural) where

    type Exact (Sum Natural) = Sum (Ratio Natural)

--------------------------------------------------------------------------------
-- Roundable
--------------------------------------------------------------------------------

class Roundable a b where
    roundD :: a -> b
    roundU :: a -> b

instance Roundable (Ratio Natural) Natural where
    roundD = floor
    roundU = ceiling

instance Roundable (Sum (Ratio Natural)) (Sum Natural) where
    roundD = fmap floor
    roundU = fmap ceiling

{-
--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------
{-
class
    ( Apportion a
    , Roundable (Fraction a) a
    , Semigroup (Fraction a)
    ) =>
    BalancedApportion a
  where
    type Fraction a
    apportionExact :: a -> NonEmpty (Weight a) -> Apportionment (Fraction a)
    apportionOrder :: a -> a -> Bool

balancedApportionLaw_length
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_length a ws =
    length (portions (apportionExact a ws)) == length ws

balancedApportionLaw_order_roundD
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_order_roundD a ws =
    zipAll apportionOrder (roundD <$> apportionExact a ws) (apportion a ws)

balancedApportionLaw_order_roundU
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_order_roundU a ws =
    zipAll apportionOrder (apportion a ws) (roundU <$> apportionExact a ws)

balancedApportionLaw_sum_roundD
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_sum_roundD a ws =
    roundD (fold1 (apportionExact a ws)) == a

balancedApportionLaw_sum_roundU
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_sum_roundU a ws =
    roundU (fold1 (apportionExact a ws)) == a
-}
--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------
{-
instance Apportion (Sum Natural) where
    type Weight (Sum Natural) = Natural
    apportionMaybe (Sum n) = fmap (fmap Sum) . Natural.apportion n-}
{-
instance BalancedApportion (Sum Natural) where
    type Fraction (Sum Natural) = Sum (Ratio Natural)
    apportionExact (Sum n) ws
        | total == 0 = Apportionment (Sum (n % 1)) (Sum (0 % 1)     <$  ws)
        | otherwise  = Apportionment (Sum (0 % 1)) (Sum . (% total) <$> ws)
      where
        total = sum ws
    apportionOrder = (<=)
-}
--------------------------------------------------------------------------------
-- Instances: List
--------------------------------------------------------------------------------
{-
instance Eq a => Apportion [a] where
    type Weight [a] = Int
    apportionMaybe as ws = do
        chunkLengths <- maybeChunkLengths
        Just $ NE.unfoldr makeChunk (chunkLengths, as)
      where
        maybeChunkLengths :: Maybe (NonEmpty Int)
        maybeChunkLengths =
            fmap (fromIntegral @Natural @Int . getSum) <$>
            apportionMaybe
                (Sum $ fromIntegral $ length as)
                (fromIntegral <$> ws)

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

instance Eq a => Apportion [a] where
    type Weight [a] = Natural
    apportion = undefined
    apportionMaybe = undefined

instance Eq a => BalancedApportion [a] where
    type Fraction [a] = Ratio Natural
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map
--------------------------------------------------------------------------------
{-
instance (Ord k, Eq v) => Apportion (Map k v) where
    type Weight (Map k v) = Map k v
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v, Roundable (Fraction v)) => BalancedApportion (Map k v) where
    type Fraction (Map k v) = Fraction v
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map Keys
--------------------------------------------------------------------------------
{-
newtype Keys a = Keys
    { unKeys :: a }
    deriving newtype (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v) => Apportion (Keys (Map k v)) where
    type Weight (Keys (Map k v)) = Natural
    apportion = undefined
    apportionMaybe = undefined

instance (Ord k, Eq v) => BalancedApportion (Keys (Map k v)) where
    type Fraction (Keys (Map k v)) = Ratio Natural
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Instances: Map Values
--------------------------------------------------------------------------------
{-
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
    type Fraction (Values (Map k v)) = Fraction v
    apportionExact = undefined
-}
--------------------------------------------------------------------------------
-- Apportioning with equal weights
--------------------------------------------------------------------------------
{-
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
-}
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

distanceRatioNatural :: Ratio Natural -> Ratio Natural -> Ratio Natural
distanceRatioNatural = undefined

naturalToRatio :: Natural -> Ratio Natural
naturalToRatio = fromIntegral

naturalLength :: Foldable f => f a -> Natural
naturalLength = fromIntegral @Int @Natural . length
{-
apportionEqualMapKeys
    :: (Ord k, Eq v) => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapKeys m ws = unKeys <$> apportionEqual (Keys m) ws

apportionEqualMapValues
    :: (Ord k, Eq v, BalancedApportion v, Integral (Weight v))
    => Map k v -> NonEmpty void -> NonEmpty (Map k v)
apportionEqualMapValues m ws = unValues <$> apportionEqual (Values m) ws
-}
{-apportionEqualNatural
    :: Sum Natural -> NonEmpty void -> NonEmpty (Sum Natural)
apportionEqualNatural = apportionEqual-}
-}
--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

mid :: (l, m, r) -> m
mid (_, m, _) = m

folds :: Semigroup a => NonEmpty a -> [NonEmpty a]
folds = undefined

splits :: Monoid a => NonEmpty a -> NonEmpty (a, a, a)
splits as = NE.zip3 ls as rs
  where
    ls = NE.scanl (<>) mempty as
    rs = NE.scanr (<>) mempty (NE.tail as)

zipAll :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAll f xs ys = all (uncurry f) (zip xs ys)

zipAny :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAny f xs ys = any (uncurry f) (zip xs ys)
