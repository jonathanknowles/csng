{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.QuickCheck.Instances.NonEmpty
    where

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.QuickCheck
    ( Arbitrary (..), shrinkMap )

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = shrinkMap (uncurry (:|)) (\(a :| as) -> (a, as))
