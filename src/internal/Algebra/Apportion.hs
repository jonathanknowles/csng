{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}

module Algebra.Apportion
    (
    -- * Apportionment
      Apportionment (..)

    -- * Apportion
    , Apportion (..)
    , apportionLaws

    -- * BoundedApportion
    , BoundedApportion (..)
    , boundedApportionLaws
    , boundedApportionAsExact
    , boundedApportionIsExact
    , boundedApportionIsBounded
    , boundedApportionLowerBound
    , boundedApportionUpperBound

    -- * CommutativeApportion
    , CommutativeApportion
    , commutativeApportionLaws

    -- * ExactApportion
    , ExactApportion
    , exactApportionLaws

    -- * Combinator functions
    , apportionJust
    , apportionList
    , apportionListMaybe
    , apportionMap
    , apportionSliceable

    -- * Combinator types
    , Size (..)
    )
    where

import Algebra.ExactBounded
    ( ExactBounded (..) )
import Algebra.PartialOrd.Extended
    ( Infix (..), PartialOrd (..) )
import Data.Bifunctor
    ( bimap )
import Data.List.Fraction
    ( ListFraction )
import Data.List.NonEmpty.Extended
    ( NonEmpty (..) )
import Data.Maybe
    ( isJust )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Monus.Extended
    ()
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy )
import Data.Ratio
    ( Ratio )
import Data.Semialign
    ( Semialign (..), Zip (..), salign )
import Data.Semigroup.Cancellative
    ( Commutative )
import Data.Sliceable
    ( Sliceable (..), takeMany )
import Data.Strict.Set
    ( Set )
import Data.These
    ( These (..) )
import Data.Traversable.Extended
    ( fill, mapAccumL )
import Data.Tuple
    ( swap )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )
import Test.QuickCheck
    ( Arbitrary (..), property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Instances.NonEmpty
    ()

import Prelude hiding
    ( last, null, splitAt, zip, zipWith )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty.Extended as NE
import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- Apportionment
--------------------------------------------------------------------------------

data Apportionment t a = Apportionment
    { remainder :: a
    , partition :: t a
    }
    deriving stock (Eq, Foldable, Functor, Show, Traversable)

instance Semialign t => Semialign (Apportionment t) where
    alignWith f a0 a1 = Apportionment
        { remainder =           f $ These (remainder a0) (remainder a1)
        , partition = alignWith f         (partition a0) (partition a1)
        }

instance Zip t => Zip (Apportionment t) where
    zipWith f a0 a1 = Apportionment
        { remainder =         f (remainder a0) (remainder a1)
        , partition = zipWith f (partition a0) (partition a1)
        }

instance (Eq (t a), Foldable t, Zip t, PartialOrd a) =>
    PartialOrd (Apportionment t a)
  where
    leq a0 a1 = and $ zipWith leq a0 a1

--------------------------------------------------------------------------------
-- Apportion
--------------------------------------------------------------------------------

class (Eq a, PositiveMonoid a, PositiveMonoid (Weight a)) => Apportion a
  where
    type Weight a

    apportion
        :: Traversable t => a -> t (Weight a) -> Apportionment t a
    default apportion
        :: Traversable t => a -> t (Weight a) -> Apportionment t a
    apportion a ws = case apportionMaybe a ws of
        Nothing -> Apportionment a (mempty <$ ws)
        Just bs -> Apportionment mempty bs

    apportionMaybe
        :: Traversable t => a -> t (Weight a) -> Maybe (t a)
    default apportionMaybe
        :: Traversable t => a -> t (Weight a) -> Maybe (t a)
    apportionMaybe a ws = case apportion a ws of
       Apportionment r bs | r == mempty -> Just bs
       _ -> Nothing

    {-# MINIMAL apportion | apportionMaybe #-}

apportionLaws
    :: forall t a.
        ( Apportion a
        , Arbitrary a
        , Arbitrary (t (Weight a))
        , Eq (t a)
        , Show a
        , Show (t (Weight a))
        , Traversable t
        )
    => Proxy a
    -> Laws
apportionLaws _ = Laws "Apportion"
    [ ( "apportionLaw_fill"
      , (apportionLaw_fill @a @t & property)
      )
    , ( "apportionLaw_fold"
      , (apportionLaw_fold @a @t & property)
      )
    , ( "apportionLaw_length"
      , (apportionLaw_length @a @t & property)
      )
    , ( "apportionLaw_maybe"
      , (apportionLaw_maybe @a @t & property)
      )
    ]

apportionLaw_fill
    :: (Apportion a, Traversable t, Eq (t a)) => a -> t (Weight a) -> Bool
apportionLaw_fill a ws =
    fill (partition (apportion a ws)) ws == partition (apportion a ws)

apportionLaw_fold
    :: (Apportion a, Traversable t) => a -> t (Weight a) -> Bool
apportionLaw_fold a ws =
    F.fold (apportion a ws) == a

apportionLaw_length
    :: (Apportion a, Traversable t) => a -> t (Weight a) -> Bool
apportionLaw_length a ws =
    length (apportionJust a ws) == length ws

apportionLaw_maybe
    :: (Apportion a, Traversable t) => a -> t (Weight a) -> Bool
apportionLaw_maybe a ws =
    isJust (apportionMaybe a (F.toList ws)) == null (remainder (apportion a ws))

--------------------------------------------------------------------------------
-- BoundedApportion
--------------------------------------------------------------------------------

class
    ( Apportion a
    , ExactApportion (Exact a)
    , ExactBounded (Exact a) a
    , ExactBounded (Weight (Exact a)) (Weight a)
    ) =>
    BoundedApportion a
  where
    type Exact a

boundedApportionAsExact
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t (Exact a)
boundedApportionAsExact a ws = apportion (exact a) (exact <$> ws)

boundedApportionIsExact
    :: (Eq (t a), Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
boundedApportionIsExact a ws = (==)
    (boundedApportionLowerBound a ws)
    (boundedApportionUpperBound a ws)

boundedApportionIsBounded
    :: (Eq (t a), Traversable t, Zip t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
boundedApportionIsBounded a ws = (&&)
    (boundedApportionLowerBound a ws `leq` apportion           a ws)
    (apportion           a ws `leq` boundedApportionUpperBound a ws)

boundedApportionLowerBound
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t a
boundedApportionLowerBound a ws = lowerBound <$> boundedApportionAsExact a ws

boundedApportionUpperBound
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t a
boundedApportionUpperBound a ws = upperBound <$> boundedApportionAsExact a ws

boundedApportionLaws
    :: forall t a.
        ( Arbitrary a
        , Arbitrary (t (Weight a))
        , BoundedApportion a
        , Eq (t a)
        , Show a
        , Show (t (Weight a))
        , Traversable t
        , Zip t
        )
    => Proxy a
    -> Laws
boundedApportionLaws _ = Laws "BoundedApportion"
    [ ( "boundedApportionLaw_isBounded"
      , (boundedApportionLaw_isBounded @a @t & property)
      )
    ]

boundedApportionLaw_isBounded
    :: forall a t. (Eq (t a), Traversable t, Zip t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
boundedApportionLaw_isBounded =
    boundedApportionIsBounded

--------------------------------------------------------------------------------
-- CommutativeApportion
--------------------------------------------------------------------------------

class (Apportion a, Commutative a, Commutative (Weight a)) =>
    CommutativeApportion a

commutativeApportionLaws
    :: forall t a.
        ( Arbitrary a
        , Arbitrary (t (Weight a))
        , CommutativeApportion a
        , Show a
        , Show (t (Weight a))
        , Traversable t
        )
    => Proxy a
    -> Laws
commutativeApportionLaws _ = Laws "CommutativeApportion"
    [ ( "commutativeApportionLaw_permutations"
      , (commutativeApportionLaw_permutations @a @t & property)
      )
    ]

commutativeApportionLaw_permutations
    :: forall a t. (Traversable t, CommutativeApportion a)
    => a
    -> t (Weight a)
    -> Bool
commutativeApportionLaw_permutations a ws =
    permutations (apportionJust a ws) == (apportionJust a <$> permutations ws)

--------------------------------------------------------------------------------
-- ExactApportion
--------------------------------------------------------------------------------

class Apportion a => ExactApportion a

exactApportionLaws
    :: forall t a.
        ( Arbitrary a
        , Arbitrary (t (Weight a))
        , ExactApportion a
        , Show a
        , Show (t (Weight a))
        , Traversable t
        )
    => Proxy a
    -> Laws
exactApportionLaws _ = Laws "ExactApportion"
    [ ( "exactApportionLaw_folds"
      , (exactApportionLaw_folds @a @t & property)
      )
    ]

exactApportionLaw_folds
    :: forall a t. (Traversable t, ExactApportion a)
    => a
    -> t (Weight a)
    -> Bool
exactApportionLaw_folds a ws =
    folds (apportionJust a ws) == (apportionJust a <$> folds ws)

--------------------------------------------------------------------------------
-- Combinator functions
--------------------------------------------------------------------------------

apportionJust :: (Traversable t, Apportion a) => a -> t (Weight a) -> t a
apportionJust a ws = partition (apportion a ws)

apportionList
    :: (Traversable t, Monoid a)
    => (a ->   [Weight a] -> Apportionment [] a)
    -> (a -> t (Weight a) -> Apportionment t  a)
apportionList f a ws = case f a (F.toList ws) of
    Apportionment r as -> Apportionment r (fill as ws)

apportionListMaybe
    :: (Traversable t, Monoid a)
    => (a ->   [Weight a] -> Maybe   [a])
    -> (a -> t (Weight a) -> Maybe (t a))
apportionListMaybe f a ws = case f a (F.toList ws) of
    Nothing -> Nothing
    Just as -> Just $ fill as ws

apportionMap
    :: (Apportion a2, Traversable t, a2 ~ Weight a2)
    => (a2 -> a3)
    -> (a1 -> a2)
    -> a1
    -> t a1
    -> Apportionment t a3
apportionMap from to = apportionMap2 from to to

apportionMap2
    :: (Apportion a2, Traversable t, w2 ~ Weight a2)
    => (a2 -> a3)
    -> (a1 -> a2)
    -> (w1 -> w2)
    -> a1
    -> t w1
    -> Apportionment t a3
apportionMap2 from toTarget toWeight a ws =
    from <$> apportion (toTarget a) (toWeight <$> ws)

apportionSliceable
    :: Traversable t
    => (Monoid a, Sliceable a, Apportion (Size (SliceableSize a)))
    => a
    -> t (Weight (Size (SliceableSize a)))
    -> Apportionment t a
apportionSliceable a ws =
    case sizes of
        Nothing -> Apportionment a (mempty <$ ws)
        Just zs -> Apportionment mempty (takeMany zs a)
  where
    sizes = fmap getSize <$> apportionMaybe (Size $ size a) ws

--------------------------------------------------------------------------------
-- Combinator types
--------------------------------------------------------------------------------

type NaturalRatio = Ratio Natural
type NaturalRatioSize = Size NaturalRatio
type NaturalRatioSum = Sum NaturalRatio
type NaturalSize = Size Natural
type NaturalSum = Sum Natural

newtype Size a = Size {getSize :: a}
    deriving stock (Eq, Generic)
    deriving (Read, Show) via (Quiet (Size a))
    deriving newtype Sliceable

--------------------------------------------------------------------------------
-- Instances: MonoidMap
--------------------------------------------------------------------------------

instance (Ord k, Apportion v, Weight v ~ v) => Apportion (MonoidMap k v)
  where
    type Weight (MonoidMap k v) = MonoidMap k v
    apportion = apportionList apportionInner
      where
        apportionInner m ms =
            F.foldl' salign empty $ apportionForKey <$> F.toList allKeys
          where
            allKeys :: Set k
            allKeys = F.foldMap MonoidMap.keys (m : F.toList ms)

            empty :: Apportionment [] (MonoidMap k v)
            empty = Apportionment mempty (mempty <$ F.toList ms)

            apportionForKey :: k -> Apportionment [] (MonoidMap k v)
            apportionForKey k = MonoidMap.singleton k <$>
                apportion (MonoidMap.get k m) (MonoidMap.get k <$> F.toList ms)

instance (Ord k, Apportion v, Weight v ~ v) => ExactApportion (MonoidMap k v)

--------------------------------------------------------------------------------
-- Instances: NaturalRatioSize
--------------------------------------------------------------------------------

deriving via NaturalRatioSum instance Semigroup      NaturalRatioSize
deriving via NaturalRatioSum instance Monoid         NaturalRatioSize
deriving via NaturalRatioSum instance MonoidNull     NaturalRatioSize
deriving via NaturalRatioSum instance PartialOrd     NaturalRatioSize
deriving via NaturalRatioSum instance PositiveMonoid NaturalRatioSize

instance Apportion NaturalRatioSize where
    type Weight NaturalRatioSize = Size NaturalRatio
    apportion = apportionMap (Size . getSum) (Sum . getSize)

instance ExactBounded NaturalRatioSize NaturalSize where
    exact (Size n) = Size (exact n)
    lowerBound (Size r) = Size (lowerBound r)
    upperBound (Size r) = Size (upperBound r)

--------------------------------------------------------------------------------
-- Instances: NaturalSize
--------------------------------------------------------------------------------

deriving via NaturalSum instance Semigroup      NaturalSize
deriving via NaturalSum instance Monoid         NaturalSize
deriving via NaturalSum instance MonoidNull     NaturalSize
deriving via NaturalSum instance PartialOrd     NaturalSize
deriving via NaturalSum instance PositiveMonoid NaturalSize

instance Apportion NaturalSize where
    type Weight NaturalSize = Size Natural
    apportion = apportionMap (Size . getSum) (Sum . getSize)

--------------------------------------------------------------------------------
-- Instances: NaturalSum
--------------------------------------------------------------------------------

instance Apportion NaturalSum where
    type Weight NaturalSum = NaturalSum
    apportion = (fmap . fmap) carryR boundedApportionAsExact

carryR :: Traversable s => s NaturalRatioSum -> s NaturalSum
carryR = snd . mapAccumL (fmap splitNaturalPart . (<>)) mempty

splitNaturalPart :: NaturalRatioSum -> (NaturalRatioSum, NaturalSum)
splitNaturalPart = bimap Sum Sum . swap . properFraction . getSum

instance BoundedApportion NaturalSum where
    type Exact NaturalSum = NaturalRatioSum

--------------------------------------------------------------------------------
-- Instances: NaturalRatioSum
--------------------------------------------------------------------------------

instance Apportion NaturalRatioSum where
    type Weight NaturalRatioSum = NaturalRatioSum
    apportion a ws
        | weightSum == mempty = Apportionment a (mempty <$ ws)
        | otherwise = Apportionment mempty (mkPortion <$> ws)
      where
        weightSum = F.fold ws
        mkPortion w = Sum (getSum a * getSum w / getSum weightSum)

instance ExactApportion NaturalRatioSum
instance CommutativeApportion NaturalRatioSum

--------------------------------------------------------------------------------
-- Instances: Size []
--------------------------------------------------------------------------------

deriving newtype instance Eq a => Semigroup      (Size [a])
deriving newtype instance Eq a => Monoid         (Size [a])
deriving newtype instance Eq a => MonoidNull     (Size [a])
deriving newtype instance Eq a => PositiveMonoid (Size [a])

deriving via Infix [a] instance Eq a => PartialOrd (Size [a])

instance Eq a => ExactBounded (Size (ListFraction a)) (Size [a]) where
    exact (Size n) = Size (getInfix $ exact $ Infix n)
    lowerBound (Size r) = Size (getInfix $ lowerBound $ Infix r)
    upperBound (Size r) = Size (getInfix $ upperBound $ Infix r)

instance Eq a => Apportion (Size [a]) where
    type Weight (Size [a]) = Size Natural
    apportion = apportionSliceable

instance Eq a => BoundedApportion (Size [a]) where
    type Exact (Size [a]) = Size (ListFraction a)

--------------------------------------------------------------------------------
-- Instances: Size ListFraction
--------------------------------------------------------------------------------

deriving newtype instance Eq a => Semigroup      (Size (ListFraction a))
deriving newtype instance Eq a => Monoid         (Size (ListFraction a))
deriving newtype instance Eq a => MonoidNull     (Size (ListFraction a))
deriving newtype instance Eq a => PositiveMonoid (Size (ListFraction a))

deriving via Infix (ListFraction a) instance Eq a =>
    PartialOrd (Size (ListFraction a))

instance Eq a => Apportion (Size (ListFraction a)) where
    type Weight (Size (ListFraction a)) = Size NaturalRatio
    apportion = apportionSliceable

instance Eq a => ExactApportion (Size (ListFraction a))

--------------------------------------------------------------------------------
-- Instances: Size Set
--------------------------------------------------------------------------------

deriving newtype instance Ord a => Semigroup      (Size (Set a))
deriving newtype instance Ord a => Monoid         (Size (Set a))
deriving newtype instance Ord a => MonoidNull     (Size (Set a))
deriving newtype instance Ord a => PositiveMonoid (Size (Set a))

instance Ord a => Apportion (Size (Set a)) where
    type Weight (Size (Set a)) = Size Natural
    apportion = apportionSliceable

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

folds :: (Traversable t, Semigroup a) => t a -> [NonEmpty a]
folds as = case F.toList as of
    [] -> []
    (x : xs) -> F.toList $ NE.folds (x :| xs)

permutations :: (Traversable t, Semigroup a) => t a -> [NonEmpty a]
permutations as = case F.toList as of
    [] -> []
    (x : xs) -> F.toList $ NE.permutations (x :| xs)
