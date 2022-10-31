{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

import Data.Coerce
    ( coerce )
import Data.Foldable
    ( foldrM )
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
    ( last, zip, zipWith )

import qualified Data.List as L
import qualified Data.List.NonEmpty.Extended as NE
import qualified Algebra.Apportion.Natural as Natural

--------------------------------------------------------------------------------
-- Partition
--------------------------------------------------------------------------------

data Partition a = Partition
    { remainder :: a
    , partition :: NonEmpty a
    }
    deriving stock (Eq, Foldable, Functor, Show)
    deriving anyclass Foldable1

instance Semialign Partition where
    alignWith f a0 a1 = Partition
        { remainder =           f $ These (remainder a0) (remainder a1)
        , partition = alignWith f         (partition a0) (partition a1)
        }

instance Zip Partition where
    zipWith f a0 a1 = Partition
        { remainder =         f (remainder a0) (remainder a1)
        , partition = zipWith f (partition a0) (partition a1)
        }

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, Semigroup a, Semigroup (Weight a)) => Apportion a where

    type Weight a

    apportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default apportion :: Monoid a
        => a -> NonEmpty (Weight a) -> Partition a
    apportion a as = case apportionMaybe a as of
        Nothing -> Partition a (mempty <$ as)
        Just bs -> Partition mempty bs

    apportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default apportionMaybe :: Monoid a
        => a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       Partition b bs | b == mempty -> Just bs
       _ -> Nothing

apportionLaw_length :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_length a ws =
    length (partition (apportion a ws)) == length ws

apportionLaw_maybe :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_maybe a ws =
    isJust (apportionMaybe a ws) == (fold1 (partition (apportion a ws)) == a)

apportionLaw_sum :: Apportion a => a -> NonEmpty (Weight a) -> Bool
apportionLaw_sum a ws =
    fold1 (apportion a ws) == a

--------------------------------------------------------------------------------
-- BalancedApportion
--------------------------------------------------------------------------------

class (Apportion a, ExactApportion (Exact a), Roundable (Exact a) (Rounded a))
    => BalancedApportion a
  where
    type Exact a
    type Rounded a

    balancedApportionOrdered
        :: Rounded a -> Rounded a -> Bool
    balancedApportionToExact
        :: a -> Exact a
    balancedApportionToExactWeight
        :: Weight a -> Weight (Exact a)
    balancedApportionToRounded
        :: a -> Rounded a

    balancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default balancedApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    balancedApportion = apportion

    balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default balancedApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    balancedApportionMaybe = apportionMaybe

balancedApportionLowerBound
    :: forall a. BalancedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Partition (Rounded a)
balancedApportionLowerBound a ws = roundD <$> exactApportion
    (balancedApportionToExact a)
    (balancedApportionToExactWeight @a <$> ws)

balancedApportionUpperBound
    :: forall a. BalancedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Partition (Rounded a)
balancedApportionUpperBound a ws = roundU <$> exactApportion
    (balancedApportionToExact a)
    (balancedApportionToExactWeight @a <$> ws)

balancedApportionRounded
    :: forall a. BalancedApportion a
    => a
    -> NonEmpty (Weight a)
    -> Partition (Rounded a)
balancedApportionRounded a ws = balancedApportionToRounded <$> apportion a ws

balancedApportionLaw_lowerBound
    :: forall a. BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_lowerBound a ws =
    and $ zipWith (balancedApportionOrdered @a)
        (balancedApportionLowerBound a ws)
        (balancedApportionRounded a ws)

balancedApportionLaw_upperBound
    :: forall a. BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_upperBound a ws =
    and $ zipWith (balancedApportionOrdered @a)
        (balancedApportionRounded a ws)
        (balancedApportionUpperBound a ws)

balancedApportionLaw_identity
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity a ws =
    balancedApportion a ws == apportion a ws

balancedApportionLaw_identity_maybe
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_identity_maybe a ws =
    balancedApportionMaybe a ws == apportionMaybe a ws

--------------------------------------------------------------------------------
-- ExactApportion
--------------------------------------------------------------------------------

class Apportion a => ExactApportion a where

    exactApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    default exactApportion
        :: a -> NonEmpty (Weight a) -> Partition a
    exactApportion = apportion

    exactApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    default exactApportionMaybe
        :: a -> NonEmpty (Weight a) -> Maybe (NonEmpty a)
    exactApportionMaybe = apportionMaybe

exactApportionLaw_identity
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_identity a ws =
    exactApportion a ws == apportion a ws

exactApportionLaw_identity_maybe
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_identity_maybe a ws =
    exactApportionMaybe a ws == apportionMaybe a ws

exactApportionLaw_folds
    :: ExactApportion a => a -> NonEmpty (Weight a) -> Bool
exactApportionLaw_folds a ws =
    folds (partition (apportion a ws)) == (partition . apportion a <$> folds ws)

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

instance ExactApportion (Sum (Ratio Natural))

--------------------------------------------------------------------------------
-- Instances: Sum Natural
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where

    type Weight (Sum Natural) = Sum Natural

    apportionMaybe = coerce Natural.apportion

instance BalancedApportion (Sum Natural) where

    type Exact (Sum Natural) = Sum (Ratio Natural)
    type Rounded (Sum Natural) = Sum Natural

    balancedApportionOrdered = (<=)
    balancedApportionToExact (Sum a) = fromIntegral a
    balancedApportionToExactWeight (Sum a) = fromIntegral a
    balancedApportionToRounded (Sum a) = Sum a

--------------------------------------------------------------------------------
-- Instances: [a]
--------------------------------------------------------------------------------

instance Eq a => Apportion [a] where

    type Weight [a] = Sum Natural

    apportionMaybe as ws = do
        chunkLengths <- maybeChunkLengths
        Just $ NE.unfoldr makeChunk (chunkLengths, as)
      where
        maybeChunkLengths :: Maybe (NonEmpty Int)
        maybeChunkLengths =
            fmap (fromIntegral @Natural @Int . getSum) <$>
            apportionMaybe
                (Sum $ fromIntegral @Int @Natural $ length as)
                (fromIntegral . getSum <$> ws)

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

instance (Eq a, Ord a) => BalancedApportion [a] where

    type Exact [a] = Sum (Ratio Natural)
    type Rounded [a] = Sum Natural

    balancedApportionOrdered = (<=)
    balancedApportionToExact = fromIntegral . length
    balancedApportionToExactWeight = fromIntegral . length
    balancedApportionToRounded = fromIntegral . length

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
    apportionExact :: a -> NonEmpty (Weight a) -> Partition (Fraction a)
    apportionOrder :: a -> a -> Bool

balancedApportionLaw_length
    :: BalancedApportion a => a -> NonEmpty (Weight a) -> Bool
balancedApportionLaw_length a ws =
    length (partition (apportionExact a ws)) == length ws

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
        | total == 0 = Partition (Sum (n % 1)) (Sum (0 % 1)     <$  ws)
        | otherwise  = Partition (Sum (0 % 1)) (Sum . (% total) <$> ws)
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

folds :: Semigroup a => NonEmpty a -> [NonEmpty a]
folds ws = case NE.splitLast ws of
    Just (prefix, last) ->
        foldrM acc (NE.singleton last) prefix
    Nothing ->
        [ws]
  where
    acc a (x :| xs) = [a <> x :| xs, a :| x : xs]

zipAll :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAll f xs ys = all (uncurry f) (zip xs ys)

zipAny :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAny f xs ys = any (uncurry f) (zip xs ys)
