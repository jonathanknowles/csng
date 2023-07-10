{- HLINT ignore "Use camelCase" -}

module Algebra.Apportion.Old.Balanced
    where

import Algebra.Apportion.Old.Apportion
    ( Apportion (..) )
import Control.Arrow
    ( (&&&) )
import Data.Function
    ( on )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy )
import Data.Semigroup.Cancellative
    ( LeftReductive (..) )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , Testable
    , arbitrary
    , checkCoverage
    , cover
    , forAllShrink
    , property
    , shrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Foldable.Extended as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class BalancedApportion a where
    balancedApportion :: a -> NonEmpty void -> NonEmpty a
    balancedApportionDistance :: a -> a -> Natural
    balancedApportionOrdering :: a -> a -> Bool

balancedApportionN :: BalancedApportion a => a -> Int -> NonEmpty a
balancedApportionN a n =
    balancedApportion a (() :| replicate (max 0 (n - 1)) ())

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

balancedApportionLaw_distance
    :: BalancedApportion a => a -> NonEmpty void -> Bool
balancedApportionLaw_distance a count =
    all ((<= 1) . uncurry balancedApportionDistance)
        (F.orderedPairs (balancedApportion a count))

balancedApportionLaw_length
    :: BalancedApportion a => a -> NonEmpty void -> Bool
balancedApportionLaw_length a count =
    length (balancedApportion a count) == length count

balancedApportionLaw_ordering
    :: BalancedApportion a => a -> NonEmpty void -> Bool
balancedApportionLaw_ordering a count =
    all (uncurry balancedApportionOrdering)
        (F.orderedPairs (balancedApportion a count))

balancedApportionLaw_sum
    :: (Eq a, BalancedApportion a, Monoid a) => a -> NonEmpty void -> Bool
balancedApportionLaw_sum a count =
    F.fold (balancedApportion a count) == a

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

bipartition :: BalancedApportion a => a -> (a, a)
bipartition = (NE.head &&& NE.last) . flip balancedApportion (() :| [()])

bipartitionUntil
    :: (Eq a, BalancedApportion a, Monoid a) => a -> (a -> Bool) -> NonEmpty a
bipartitionUntil a f
    | x == mempty = pure a
    | y == a      = pure a
    | f a         = pure a
    | otherwise   = (`bipartitionUntil` f) =<< (x :| [y])
  where
    (x, y) = bipartition a

bipartitionWhile
    :: (Eq a, BalancedApportion a, Monoid a) => a -> (a -> Bool) -> NonEmpty a
bipartitionWhile a f
    | x == mempty = pure a
    | y == a      = pure a
    | f a         = (`bipartitionWhile` f) =<< (x :| [y])
    | otherwise   = pure a
  where
    (x, y) = bipartition a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

newtype Keys a = Keys
    { unKeys :: a }
    deriving (Eq, Monoid, Semigroup, Show)

newtype Values a = Values
    { unValues :: a }
    deriving (Eq, Monoid, Semigroup, Show)

instance (Ord k, Eq v, MonoidNull v) => BalancedApportion (Keys (MonoidMap k v))
  where
    balancedApportion m
        = fmap (Keys . MonoidMap.fromMap)
        . balancedApportion (MonoidMap.toMap $ unKeys m)
    balancedApportionDistance
        = balancedApportionDistance `on` MonoidMap.toMap
        . unKeys
    balancedApportionOrdering
        = balancedApportionOrdering `on` MonoidMap.toMap
        . unKeys

instance (Ord k, Eq v, BalancedApportion v, MonoidNull v, LeftReductive v) =>
    BalancedApportion (Values (MonoidMap k v))
  where
    balancedApportion (Values m) count =
        Values <$> F.foldl' acc (mempty <$ count) (toList m)
      where
        acc :: NonEmpty (MonoidMap k v) -> (k, v) -> NonEmpty (MonoidMap k v)
        acc ms (k, v) = NE.zipWith (<>) ms $
            MonoidMap.singleton k <$> balancedApportion v count

    balancedApportionDistance (Values m1) (Values m2) =
        maybe 0 maximum (NE.nonEmpty distances)
      where
        allKeys :: Set k
        allKeys = MonoidMap.nonNullKeys m1 <> MonoidMap.nonNullKeys m2

        distances :: [Natural]
        distances = distanceForKey <$> F.toList allKeys

        distanceForKey :: k -> Natural
        distanceForKey k =
            MonoidMap.get k m1 `balancedApportionDistance` MonoidMap.get k m2

    balancedApportionOrdering (Values m1) (Values m2) =
        m1 `isPrefixOf` m2

instance BalancedApportion Natural where
    balancedApportion n count = snd (apportion n (1 <$ count))

    balancedApportionDistance n1 n2
        | n1 >= n2  = n1 - n2
        | otherwise = n2 - n1

    balancedApportionOrdering n1 n2 = n1 <= n2

instance BalancedApportion [a] where
    balancedApportion as count =
        NE.unfoldr makeChunk (chunkLengths, as)
      where
        chunkLengths :: NonEmpty Int
        chunkLengths = fromIntegral @Natural @Int <$>
            balancedApportion (fromIntegral @Int @Natural (length as)) count

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

    balancedApportionDistance xs ys = balancedApportionDistance
        (fromIntegral @Int @Natural $ length xs)
        (fromIntegral @Int @Natural $ length ys)

    balancedApportionOrdering xs ys = length xs <= length ys

instance Ord k => BalancedApportion (Map k v) where
    balancedApportion m count =
        Map.fromList <$> balancedApportion (Map.toList m) count

    balancedApportionDistance m1 m2 = balancedApportionDistance
        (fromIntegral @Int @Natural $ Map.size m1)
        (fromIntegral @Int @Natural $ Map.size m2)

    balancedApportionOrdering m1 m2 = Map.size m1 <= Map.size m2

instance Ord a => BalancedApportion (Set a) where
    balancedApportion set count =
        Set.fromList <$> balancedApportion (Set.toList set) count

    balancedApportionDistance xs ys = balancedApportionDistance
        (fromIntegral @Int @Natural $ Set.size xs)
        (fromIntegral @Int @Natural $ Set.size ys)

    balancedApportionOrdering xs ys = length xs <= length ys

deriving instance BalancedApportion a => BalancedApportion (Sum a)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

balancedApportionLaws
    :: forall a. (Arbitrary a, Eq a, BalancedApportion a, Monoid a, Show a)
    => Proxy a
    -> Laws
balancedApportionLaws _ = Laws "BalancedApportion"
    [ ( "Distance"
      , makeProperty balancedApportionLaw_distance)
    , ( "Length"
      , makeProperty balancedApportionLaw_length)
    , ( "Ordering"
      , makeProperty balancedApportionLaw_ordering)
    , ( "Sum"
      , makeProperty balancedApportionLaw_sum)
    ]
  where
    makeProperty :: (a -> NonEmpty () -> Bool) -> Property
    makeProperty =
        property . forAllShrink genCount shrinkCount . makePropertyInner
      where
        genCount :: Gen (NonEmpty ())
        genCount = (:|) <$> arbitrary <*> arbitrary

        shrinkCount :: NonEmpty () -> [NonEmpty ()]
        shrinkCount = mapMaybe NE.nonEmpty . shrink . NE.toList

    makePropertyInner
        :: (a -> NonEmpty () -> Bool)
        -> NonEmpty ()
        -> (a -> Property)
    makePropertyInner condition count value =
        checkCoverage $
        buildCoverage value count result $
        condition value count
      where
        result = balancedApportion value count

    buildCoverage
        :: Testable prop
        => a
        -> NonEmpty ()
        -> NonEmpty a
        -> prop
        -> Property
    buildCoverage value count result
        = cover 1
            (length count == 1)
            "length count == 1"
        . cover 10
            (length count /= 1)
            "length count /= 1"
        . cover 1
            (value == mempty)
            "value == mempty"
        . cover 10
            (value /= mempty)
            "value /= mempty"
        . cover 1
            (NE.head result == mempty)
            "NE.head result == mempty"
        . cover 10
            (NE.head result /= mempty)
            "NE.head result /= mempty"
        . cover 1
            (NE.last result == mempty)
            "NE.last result == mempty"
        . cover 10
            (NE.last result /= mempty)
            "NE.last result /= mempty"
        . cover 1
            (NE.head result == NE.last result)
            "NE.head result == NE.last result"
        . cover 10
            (NE.head result /= NE.last result)
            "NE.head result /= NE.last result"
        . cover 1
            (balancedApportionDistance (NE.head result) (NE.last result) == 0)
            "balancedApportionDistance (NE.head result) (NE.last result) == 0"
        . cover 10
            (balancedApportionDistance (NE.head result) (NE.last result) /= 0)
            "balancedApportionDistance (NE.head result) (NE.last result) /= 0"
        . cover 1
            (all (uncurry (/=)) (F.consecutivePairs result))
            "all (uncurry (/=)) (F.consecutivePairs result)"
        . cover 1
            (all (uncurry (==)) (F.consecutivePairs result))
            "all (uncurry (==)) (F.consecutivePairs result)"
