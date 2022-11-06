{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}

module Algebra.NewApportion
    where

import Algebra.ExactBounded
    ( ExactBounded (..) )
import Algebra.PartialOrd.Extended
    ( Infix (..), PartialOrd (..) )
import Data.Foldable
    ( fold )
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
    ( MonoidNull, PositiveMonoid )
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
import Data.Sized
    ( size )
import Data.SizeDivisible
    ( SizeDivisible (..), takeMany )
import Data.Strict.Set
    ( Set )
import Data.These
    ( These (..) )
import Data.Traversable
    ( mapAccumL )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..), property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Instances.NonEmpty
    ()

import Prelude hiding
    ( last, splitAt, zip, zipWith )

import qualified Algebra.Apportion.Natural as Natural
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty.Extended as NE
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Sized as Sized

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

apportionJust :: (Traversable t, Apportion a) => a -> t (Weight a) -> t a
apportionJust a ws = partition (apportion a ws)

apportionLaws
    :: forall t a.
        ( Apportion a
        , Arbitrary a
        , Arbitrary (t (Weight a))
        , Show a
        , Show (t (Weight a))
        , Traversable t
        )
    => Proxy a
    -> Laws
apportionLaws _ = Laws "Apportion"
    [ ( "apportionLaw_fold"
      , (apportionLaw_fold @a @t & property)
      )
    , ( "apportionLaw_length"
      , (apportionLaw_length @a @t & property)
      )
    , ( "apportionLaw_maybe"
      , (apportionLaw_maybe @a @t & property)
      )
    ]

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
    isJust (apportionMaybe a (F.toList ws)) == (fold (apportion a ws) == a)

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

apportionAsExact
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t (Exact a)
apportionAsExact a ws = apportion (exact a) (exact <$> ws)

apportionIsExact
    :: (Eq (t a), Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
apportionIsExact a ws = (==)
    (apportionLowerBound a ws)
    (apportionUpperBound a ws)

apportionIsBounded
    :: (Eq (t a), Traversable t, Zip t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
apportionIsBounded a ws = (&&)
    (apportionLowerBound a ws `leq` apportion           a ws)
    (apportion           a ws `leq` apportionUpperBound a ws)

apportionLowerBound
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t a
apportionLowerBound a ws = lowerBound <$> apportionAsExact a ws

apportionUpperBound
    :: (Traversable t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Apportionment t a
apportionUpperBound a ws = upperBound <$> apportionAsExact a ws

boundedApportionLaw_isBounded
    :: (Eq (t a), Traversable t, Zip t, BoundedApportion a)
    => a
    -> t (Weight a)
    -> Bool
boundedApportionLaw_isBounded =
    apportionIsBounded

--------------------------------------------------------------------------------
-- CommutativeApportion
--------------------------------------------------------------------------------

class (Apportion a, Commutative a, Commutative (Weight a)) =>
    CommutativeApportion a

commutativeApportionLaw_permutations
    :: (Traversable t, CommutativeApportion a) => a -> t (Weight a) -> Bool
commutativeApportionLaw_permutations a ws =
    permutations (apportionJust a ws) == (apportionJust a <$> permutations ws)

--------------------------------------------------------------------------------
-- ExactApportion
--------------------------------------------------------------------------------

class Apportion a => ExactApportion a

exactApportionLaw_folds
    :: (Traversable t, ExactApportion a) => a -> t (Weight a) -> Bool
exactApportionLaw_folds a ws =
    folds (apportionJust a ws) == (apportionJust a <$> folds ws)

--------------------------------------------------------------------------------
-- Combinator types
--------------------------------------------------------------------------------

type NaturalRatio = Ratio Natural
type NaturalRatioSize = Size NaturalRatio
type NaturalRatioSum = Sum NaturalRatio
type NaturalSize = Size Natural
type NaturalSum = Sum Natural

newtype Size a = Size {getSize :: a}
    deriving stock (Eq, Show)
    deriving newtype (Sized.Sized, SizeDivisible)

--------------------------------------------------------------------------------
-- Combinator functions
--------------------------------------------------------------------------------

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

apportionSizeDivisible
    :: Traversable t
    => (Monoid a, SizeDivisible a, Apportion (Size (Sized.Size a)))
    => a
    -> t (Weight (Size (Sized.Size a)))
    -> Apportionment t a
apportionSizeDivisible a ws =
    case sizes of
        Nothing -> Apportionment a (mempty <$ ws)
        Just zs -> Apportionment mempty (takeMany zs a)
  where
    sizes = fmap getSize <$> apportionMaybe (Size $ size a) ws

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
    apportion = apportionList $ \a ws -> case NE.nonEmpty ws of
        Nothing -> Apportionment a []
        Just xs ->
            case Natural.apportion (getSum a) (getSum <$> xs) of
                Nothing -> Apportionment a []
                Just as -> Apportionment mempty (NE.toList (Sum <$> as))

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
    apportion = apportionSizeDivisible

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
    apportion = apportionSizeDivisible

instance Eq a => ExactApportion (Size (ListFraction a))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

fill :: (Monoid b, Foldable f, Traversable t) => f b -> t a -> t b
fill xs = snd . mapAccumL fillM (F.toList xs)
  where
    fillM [] _ = ([], mempty)
    fillM (y : ys) _ = (ys, y)

folds :: (Traversable t, Semigroup a) => t a -> [NonEmpty a]
folds as = case F.toList as of
    [] -> []
    (x : xs) -> F.toList $ NE.folds (x :| xs)

permutations :: (Traversable t, Semigroup a) => t a -> [NonEmpty a]
permutations as = case F.toList as of
    [] -> []
    (x : xs) -> F.toList $ NE.permutations (x :| xs)

zipAll :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAll f xs ys = all (uncurry f) (zip xs ys)

zipAny :: (Foldable t, Zip t) => (a -> b -> Bool) -> t a -> t b -> Bool
zipAny f xs ys = any (uncurry f) (zip xs ys)
