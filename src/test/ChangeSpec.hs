{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChangeSpec where

import Prelude

import Change
    ( makeChangeForAsset, makeChangeForCoin )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Monoid
    ( Sum (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck.Extended
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , cover
    , property
    , shrinkBoundedEnum
    , (===)
    )
import Test.QuickCheck.Instances.NonEmpty
    ()
import Test.QuickCheck.Quid
    ( Latin (..), Quid, Size (..) )
import Value
    ( Coin, HasAssets (..) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = do

    parallel $ describe "makeChangeForAsset" $ do
        it "prop_makeChangeForAsset_length" $
            prop_makeChangeForAsset_length
                & property
        it "prop_makeChangeForAsset_order" $
            prop_makeChangeForAsset_order
                & property
        it "prop_makeChangeForAsset_remainder" $
            prop_makeChangeForAsset_remainder
                & property
        it "prop_makeChangeForAsset_sum" $
            prop_makeChangeForAsset_sum
                & property

    parallel $ describe "makeChangeForCoin" $ do
        it "prop_makeChangeForCoin_length" $
            prop_makeChangeForCoin_length
                & property
        it "prop_makeChangeForCoin_order" $
            prop_makeChangeForCoin_order
                & property
        it "prop_makeChangeForCoin_remainder" $
            prop_makeChangeForCoin_remainder
                & property
        it "prop_makeChangeForCoin_sum" $
            prop_makeChangeForCoin_sum
                & property

prop_makeChangeForAsset_length
    :: Natural
    -> NonEmpty (TestPriority, Natural)
    -> Property
prop_makeChangeForAsset_length n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    NE.length (snd (makeChangeForAsset (n, pws))) === NE.length pws
  where
    weightSum = F.foldMap (Sum . snd) pws

prop_makeChangeForAsset_order
    :: Natural
    -> NonEmpty (TestPriority, Natural)
    -> Property
prop_makeChangeForAsset_order n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    fmap fst (snd (makeChangeForAsset (n, pws)))
        === fmap fst pws
  where
    weightSum = F.foldMap (Sum . snd) pws

prop_makeChangeForAsset_remainder
    :: Natural
    -> NonEmpty (TestPriority, Natural)
    -> Property
prop_makeChangeForAsset_remainder n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    fst (makeChangeForAsset (n, pws))
        === if weightSum == 0 then n else 0
  where
    weightSum = F.foldMap (Sum . snd) pws

prop_makeChangeForAsset_sum
    :: Natural
    -> NonEmpty (TestPriority, Natural)
    -> Property
prop_makeChangeForAsset_sum n pws =
    checkCoverage $
    cover  1 (weightSum == 0) "weightSum == 0" $
    cover 50 (weightSum /= 0) "weightSum /= 0" $
    sum (fmap snd (snd (makeChangeForAsset (n, pws))))
        === if weightSum == 0 then 0 else n
  where
    weightSum = F.foldMap (Sum . snd) pws

prop_makeChangeForCoin_length
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_length n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    NE.length (snd (makeChangeForCoin (n, pws))) === NE.length pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_order
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_order n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    fmap fst (snd (makeChangeForCoin (n, pws)))
        === fmap fst pws
  where
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_remainder
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_remainder n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    fst (makeChangeForCoin (n, pws))
        === filterAssets (`Set.notMember` weightAssets) n
  where
    weightAssets = F.foldMap getAssets (snd <$> pws)
    weightSum = F.foldMap snd pws

prop_makeChangeForCoin_sum
    :: TestCoin
    -> NonEmpty (TestPriority, TestCoin)
    -> Property
prop_makeChangeForCoin_sum n pws =
    checkCoverage $
    cover  1 (weightSum == mempty) "weightSum == mempty" $
    cover 50 (weightSum /= mempty) "weightSum /= mempty" $
    foldMap snd (snd (makeChangeForCoin (n, pws)))
        === filterAssets (`Set.member` commonAssets) n
  where
    commonAssets = weightAssets `Set.intersection` targetAssets
    targetAssets = getAssets n
    weightAssets = F.foldMap getAssets (snd <$> pws)
    weightSum = F.foldMap snd pws

type TestCoin = Coin TestAsset

newtype TestAsset = TestAsset (Latin Quid)
    deriving stock (Eq, Ord, Read, Show)
    deriving Arbitrary via (Size 4 Quid)

data TestPriority
    = TestPriority0
    | TestPriority1
    | TestPriority2
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary TestPriority where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum
