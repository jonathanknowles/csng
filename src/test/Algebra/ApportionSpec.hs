{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.ApportionSpec where

import Algebra.Apportion
    ( Size (..), apportionLaws, boundedApportionLaws )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Sum )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Extended
    ( Arbitrary (..), arbitraryBoundedEnum, shrinkBoundedEnum )
import Test.QuickCheck.Instances.Natural
    ()

spec :: Spec
spec = do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Sum Natural)
            [ apportionLaws @[]
            , apportionLaws @TestContainer
            , boundedApportionLaws @[]
            , boundedApportionLaws @TestContainer
            ]
        testLawsMany @(Size [Int])
            [ apportionLaws @[]
            , apportionLaws @TestContainer
            , boundedApportionLaws @[]
            , boundedApportionLaws @TestContainer
            ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary LatinChar where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

type TestContainer = Map LatinChar

deriving newtype instance Arbitrary a => Arbitrary (Size a)
