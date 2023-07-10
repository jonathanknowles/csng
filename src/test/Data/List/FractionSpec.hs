{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.List.FractionSpec where

import Data.List.Fraction
    ( ListFraction )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , cover
    , elements
    , listOf
    , oneof
    , property
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showLaws
    , showReadLaws
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws, positiveMonoidLaws )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Data.List.Fraction as LF

spec :: Spec
spec = do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(ListFraction TestChar)
            [ eqLaws
            , monoidLaws
            , monoidNullLaws
            , positiveMonoidLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showLaws
            , showReadLaws
            ]

    parallel $ describe "Properties" $ do
        it "prop_genLength_coverage" $
            prop_genLength_coverage @TestChar
                & property
        it "prop_genListFraction_isValid" $
            prop_genListFraction_isValid @TestChar
                & property
        it "prop_drop_isValid" $
            prop_drop_isValid @TestChar
                & property
        it "prop_drop_isInfixOf" $
            prop_drop_isInfixOf @TestChar
                & property
        it "prop_drop_isSuffixOf" $
            prop_drop_isSuffixOf @TestChar
                & property
        it "prop_take_isValid" $
            prop_take_isValid @TestChar
                & property
        it "prop_take_isInfixOf" $
            prop_take_isInfixOf @TestChar
                & property
        it "prop_take_isPrefixOf" $
            prop_take_isPrefixOf @TestChar
                & property
        it "prop_mappend_length" $
            prop_mappend_length @TestChar
                & property
        it "prop_mappend_isInfixOf" $
            prop_mappend_isInfixOf @TestChar
                & property
        it "prop_mappend_isPrefixOf" $
            prop_mappend_isPrefixOf @TestChar
                & property
        it "prop_mappend_isSuffixOf" $
            prop_mappend_isSuffixOf @TestChar
                & property

prop_genLength_coverage :: Eq a => Length -> ListFraction a -> Property
prop_genLength_coverage (Length n) f =
    checkCoverage $
    cover 1
        (n == 0)
        "n == 0" $
    cover 10
        (n > 0 && n < LF.length f)
        "n > 0 && n < LF.length f" $
    cover 0.1
        (n > 0 && n == LF.length f)
        "n > 0 && n == LF.length f" $
    cover 10
        (n > 0 && n > LF.length f)
        "n > 0 && n > LF.length f" $
    property True

prop_genListFraction_isValid
    :: Eq a => ListFraction a -> Property
prop_genListFraction_isValid f = LF.isValid f === True

prop_drop_isValid
    :: Eq a => Length -> ListFraction a -> Property
prop_drop_isValid (Length n) f =
    LF.isValid (LF.drop n f) === True

prop_drop_isInfixOf
    :: Eq a => Length -> ListFraction a -> Property
prop_drop_isInfixOf (Length n) f =
    LF.isInfixOf (LF.drop n f) f === True

prop_drop_isSuffixOf
    :: Eq a => Length -> ListFraction a -> Property
prop_drop_isSuffixOf (Length n) f =
    LF.isSuffixOf (LF.drop n f) f === True

prop_take_isValid
    :: Eq a => Length -> ListFraction a -> Property
prop_take_isValid (Length n) f =
    LF.isValid (LF.take n f) === True

prop_take_isInfixOf
    :: Eq a => Length -> ListFraction a -> Property
prop_take_isInfixOf (Length n) f =
    LF.isInfixOf (LF.take n f) f === True

prop_take_isPrefixOf
    :: Eq a => Length -> ListFraction a -> Property
prop_take_isPrefixOf (Length n) f =
    LF.isPrefixOf (LF.take n f) f === True

prop_mappend_length
    :: Eq a => ListFraction a -> ListFraction a -> Property
prop_mappend_length f1 f2 =
    Sum (LF.length (f1 <> f2)) === Sum (LF.length f1) <> Sum (LF.length f2)

prop_mappend_isInfixOf
    :: Eq a => ListFraction a -> ListFraction a -> ListFraction a -> Property
prop_mappend_isInfixOf f1 f2 f3 = f2 `LF.isInfixOf` (f1 <> f2 <> f3) === True

prop_mappend_isPrefixOf
    :: Eq a => ListFraction a -> ListFraction a -> Property
prop_mappend_isPrefixOf f1 f2 = f1 `LF.isPrefixOf` (f1 <> f2) === True

prop_mappend_isSuffixOf
    :: Eq a => ListFraction a -> ListFraction a -> Property
prop_mappend_isSuffixOf f1 f2 = f2 `LF.isSuffixOf` (f1 <> f2) === True

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

data TestChar = A | B | C | D
    deriving (Bounded, Enum, Eq, Read, Show)

instance Arbitrary TestChar where
    arbitrary = arbitraryBoundedEnum

newtype Length = Length {getLength :: Ratio Natural}
    deriving (Eq, Show)

instance Arbitrary Length where
    arbitrary = genLength

genLength :: Gen Length
genLength = Length <$> ((%) <$> arbitrary <*> elements [1 .. 4])

instance (Arbitrary a, Eq a) => Arbitrary (ListFraction a) where
    arbitrary = genListFraction arbitrary

genListFraction :: forall a. Eq a => Gen a -> Gen (ListFraction a)
genListFraction genElement =
    LF.fromFractions <$> listOf genElementFraction
  where
    genElementFraction :: Gen (a, Ratio Natural)
    genElementFraction = (,) <$> genElement <*> genFraction

    genFraction :: Gen (Ratio Natural)
    genFraction = oneof
        [ pure 1
        , (%) <$> elements [1 .. 4] <*> elements [1 .. 4]
        ]
