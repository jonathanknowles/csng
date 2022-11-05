{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Instances.NonEmpty where

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, listOf, shrinkList )

import qualified Data.List.NonEmpty as NE

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = genNonEmpty arbitrary
    shrink = shrinkNonEmpty shrink

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty genA = (:|) <$> genA <*> listOf genA

shrinkNonEmpty :: (a -> [a]) -> (NonEmpty a -> [NonEmpty a])
shrinkNonEmpty shrinkA = mapMaybe NE.nonEmpty . shrinkList shrinkA . NE.toList
