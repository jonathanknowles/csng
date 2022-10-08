{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.Equipartition
    where

import Prelude

import Control.Arrow
    ( (&&&) )
import Data.Function
    ( on )
import Data.List.NonEmpty
    ( NonEmpty (..) )
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
import Data.Strict.Map
    ( Map )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Numeric.Natural.Extra
    ( equipartitionNatural )
import Safe
    ( tailMay )
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

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set
import qualified Data.Strict.Map as Map

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class Equipartition a where
    equipartition :: a -> NonEmpty void -> NonEmpty a
    equipartitionDistance :: a -> a -> Natural
    equipartitionOrdering :: a -> a -> Bool

equipartitionN :: Equipartition a => a -> Int -> NonEmpty a
equipartitionN a n = equipartition a (() :| replicate (max 0 (n - 1)) ())

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

equipartitionLaw_distance
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_distance a count =
    all ((<= 1) . uncurry equipartitionDistance)
        (orderedPairs (equipartition a count))

equipartitionLaw_length
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_length a count =
    length (equipartition a count) == length count

equipartitionLaw_ordering
    :: Equipartition a => a -> NonEmpty void -> Bool
equipartitionLaw_ordering a count =
    all (uncurry equipartitionOrdering)
        (orderedPairs (equipartition a count))

equipartitionLaw_sum
    :: (Eq a, Equipartition a, Monoid a) => a -> NonEmpty void -> Bool
equipartitionLaw_sum a count =
    F.fold (equipartition a count) == a

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

bipartition :: Equipartition a => a -> (a, a)
bipartition = (NE.head &&& NE.last) . flip equipartition (() :| [()])

bipartitionUntil
    :: (Eq a, Equipartition a, Monoid a) => a -> (a -> Bool) -> NonEmpty a
bipartitionUntil a f
    | x == mempty = pure a
    | y == a      = pure a
    | f a         = pure a
    | otherwise   = (`bipartitionUntil` f) =<< (x :| [y])
  where
    (x, y) = bipartition a

bipartitionWhile
    :: (Eq a, Equipartition a, Monoid a) => a -> (a -> Bool) -> NonEmpty a
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

instance (Ord k, Eq v, MonoidNull v) => Equipartition (Keys (MonoidMap k v))
  where
    equipartition m
        = fmap (Keys . MonoidMap.fromMap)
        . equipartition (MonoidMap.toMap $ unKeys m)
    equipartitionDistance
        = equipartitionDistance `on` MonoidMap.toMap
        . unKeys
    equipartitionOrdering
        = equipartitionOrdering `on` MonoidMap.toMap
        . unKeys

instance (Ord k, Eq v, Equipartition v, MonoidNull v, LeftReductive v) =>
    Equipartition (Values (MonoidMap k v))
  where
    equipartition (Values m) count =
        Values <$> F.foldl' acc (mempty <$ count) (toList m)
      where
        acc :: NonEmpty (MonoidMap k v) -> (k, v) -> NonEmpty (MonoidMap k v)
        acc ms (k, v) = NE.zipWith (<>) ms $
            MonoidMap.singleton k <$> equipartition v count

    equipartitionDistance (Values m1) (Values m2) =
        maybe 0 maximum (NE.nonEmpty distances)
      where
        allKeys :: Set k
        allKeys = MonoidMap.nonNullKeys m1 <> MonoidMap.nonNullKeys m2

        distances :: [Natural]
        distances = distanceForKey <$> F.toList allKeys

        distanceForKey :: k -> Natural
        distanceForKey k =
            MonoidMap.get k m1 `equipartitionDistance` MonoidMap.get k m2

    equipartitionOrdering (Values m1) (Values m2) =
        m1 `isPrefixOf` m2

instance Equipartition Natural where
    equipartition = equipartitionNatural

    equipartitionDistance n1 n2
        | n1 >= n2  = n1 - n2
        | otherwise = n2 - n1

    equipartitionOrdering n1 n2 = n1 <= n2

instance Equipartition [a] where
    equipartition as count =
        NE.unfoldr makeChunk (chunkLengths, as)
      where
        chunkLengths :: NonEmpty Int
        chunkLengths = fromIntegral @Natural @Int <$>
            equipartition (fromIntegral @Int @Natural (length as)) count

        makeChunk :: (NonEmpty Int, [a]) -> ([a], Maybe (NonEmpty Int, [a]))
        makeChunk (c :| mcs, bs) = case NE.nonEmpty mcs of
            Just cs -> (prefix, Just (cs, suffix))
            Nothing -> (bs, Nothing)
          where
            (prefix, suffix) = L.splitAt c bs

    equipartitionDistance xs ys = equipartitionDistance
        (fromIntegral @Int @Natural $ length xs)
        (fromIntegral @Int @Natural $ length ys)

    equipartitionOrdering xs ys = length xs <= length ys

instance Ord k => Equipartition (Map k v) where
    equipartition m count =
        Map.fromList <$> equipartition (Map.toList m) count

    equipartitionDistance m1 m2 = equipartitionDistance
        (fromIntegral @Int @Natural $ Map.size m1)
        (fromIntegral @Int @Natural $ Map.size m2)

    equipartitionOrdering m1 m2 = Map.size m1 <= Map.size m2

instance Ord a => Equipartition (Set a) where
    equipartition set count =
        Set.fromList <$> equipartition (Set.toList set) count

    equipartitionDistance xs ys = equipartitionDistance
        (fromIntegral @Int @Natural $ Set.size xs)
        (fromIntegral @Int @Natural $ Set.size ys)

    equipartitionOrdering xs ys = length xs <= length ys

deriving instance Equipartition a => Equipartition (Sum a)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

equipartitionLaws
    :: forall a. (Arbitrary a, Eq a, Equipartition a, Monoid a, Show a)
    => Proxy a
    -> Laws
equipartitionLaws _ = Laws "Equipartition"
    [ ( "Distance"
      , makeProperty equipartitionLaw_distance)
    , ( "Length"
      , makeProperty equipartitionLaw_length)
    , ( "Ordering"
      , makeProperty equipartitionLaw_ordering)
    , ( "Sum"
      , makeProperty equipartitionLaw_sum)
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
        result = equipartition value count

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
            (equipartitionDistance (NE.head result) (NE.last result) == 0)
            "equipartitionDistance (NE.head result) (NE.last result) == 0"
        . cover 10
            (equipartitionDistance (NE.head result) (NE.last result) /= 0)
            "equipartitionDistance (NE.head result) (NE.last result) /= 0"
        . cover 1
            (all (uncurry (/=)) (consecutivePairs result))
            "all (uncurry (/=)) (consecutivePairs result)"
        . cover 1
            (all (uncurry (==)) (consecutivePairs result))
            "all (uncurry (==)) (consecutivePairs result)"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

consecutivePairs :: Foldable f => f a -> [(a, a)]
consecutivePairs = inner . F.toList
  where
    inner xs = case tailMay xs of
        Nothing -> []
        Just ys -> xs `zip` ys

orderedPairs :: Foldable f => f a -> [(a, a)]
orderedPairs = inner . F.toList
  where
    inner [      ] = []
    inner [_     ] = []
    inner (x : xs) = [(x, y) | y <- xs] <> inner xs
