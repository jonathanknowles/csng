{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.ApportionSpec
    where

import Algebra.Apportion
    ( Apportion (..), apportionLaws )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( Sum (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.Hspec.Unit
    ( UnitTestData2, unitTestData2, unitTestSpec )
import Test.QuickCheck
    ( Arbitrary (..), arbitrarySizedIntegral )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )

spec :: Spec
spec = do
    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Sum Natural)
            [ apportionLaws
            ]

    parallel $ describe "apportion" $ do

        describe "unit tests" $ do
            unitTestSpec_apportion_Natural

--------------------------------------------------------------------------------
-- Unit tests: Apportion Natural
--------------------------------------------------------------------------------

unitTestSpec_apportion_Natural :: Spec
unitTestSpec_apportion_Natural = unitTestSpec
    "apportion Natural"
    "apportion"
    (apportion)
    (unitTestData_apportion_Natural)

unitTestData_apportion_Natural :: UnitTestData2
    (Natural)
    (NonEmpty Natural)
    (Natural, NonEmpty Natural)
unitTestData_apportion_Natural = unitTestData2
    [ (  1, [              0], ( 1, [              0]) )
    , (  1, [          0,  0], ( 1, [          0,  0]) )
    , (  1, [      0,  0,  0], ( 1, [      0,  0,  0]) )
    , (  1, [  0,  0,  0,  0], ( 1, [  0,  0,  0,  0]) )

    , ( 99, [              0], (99, [              0]) )
    , ( 99, [          0,  0], (99, [          0,  0]) )
    , ( 99, [      0,  0,  0], (99, [      0,  0,  0]) )
    , ( 99, [  0,  0,  0,  0], (99, [  0,  0,  0,  0]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          0,  1], ( 0, [          0,  1]) )
    , (  1, [      0,  0,  1], ( 0, [      0,  0,  1]) )
    , (  1, [  0,  0,  0,  1], ( 0, [  0,  0,  0,  1]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          1,  0], ( 0, [          1,  0]) )
    , (  1, [      1,  0,  0], ( 0, [      1,  0,  0]) )
    , (  1, [  1,  0,  0,  0], ( 0, [  1,  0,  0,  0]) )

    , (  1, [              1], ( 0, [              1]) )
    , (  1, [          1,  1], ( 0, [          0,  1]) )
    , (  1, [      1,  1,  1], ( 0, [      0,  0,  1]) )
    , (  1, [  1,  1,  1,  1], ( 0, [  0,  0,  0,  1]) )

    , (  2, [              1], ( 0, [              2]) )
    , (  2, [          1,  1], ( 0, [          1,  1]) )
    , (  2, [      1,  1,  1], ( 0, [      0,  1,  1]) )
    , (  2, [  1,  1,  1,  1], ( 0, [  0,  0,  1,  1]) )

    , (  3, [              1], ( 0, [              3]) )
    , (  3, [          1,  1], ( 0, [          1,  2]) )
    , (  3, [      1,  1,  1], ( 0, [      1,  1,  1]) )
    , (  3, [  1,  1,  1,  1], ( 0, [  0,  1,  1,  1]) )

    , (  4, [              1], ( 0, [              4]) )
    , (  4, [          1,  1], ( 0, [          2,  2]) )
    , (  4, [      1,  1,  1], ( 0, [      1,  1,  2]) )
    , (  4, [  1,  1,  1,  1], ( 0, [  1,  1,  1,  1]) )

    , (  0, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  0]) )
    , (  1, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  1]) )
    , (  2, [  1,  2,  4,  8], ( 0, [  0,  0,  0,  2]) )
    , (  3, [  1,  2,  4,  8], ( 0, [  0,  0,  1,  2]) )
    , (  4, [  1,  2,  4,  8], ( 0, [  0,  0,  1,  3]) )
    , (  5, [  1,  2,  4,  8], ( 0, [  0,  0,  2,  3]) )
    , (  6, [  1,  2,  4,  8], ( 0, [  0,  0,  2,  4]) )
    , (  7, [  1,  2,  4,  8], ( 0, [  0,  1,  2,  4]) )
    , (  8, [  1,  2,  4,  8], ( 0, [  0,  1,  2,  5]) )
    , (  9, [  1,  2,  4,  8], ( 0, [  0,  1,  3,  5]) )
    , ( 10, [  1,  2,  4,  8], ( 0, [  0,  1,  3,  6]) )
    , ( 11, [  1,  2,  4,  8], ( 0, [  0,  2,  3,  6]) )
    , ( 12, [  1,  2,  4,  8], ( 0, [  0,  1,  4,  7]) )
    , ( 13, [  1,  2,  4,  8], ( 0, [  0,  2,  4,  7]) )
    , ( 14, [  1,  2,  4,  8], ( 0, [  0,  2,  4,  8]) )
    , ( 15, [  1,  2,  4,  8], ( 0, [  1,  2,  4,  8]) )
    ]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Natural where
    arbitrary = fromIntegral . abs <$> arbitrarySizedIntegral @Int
