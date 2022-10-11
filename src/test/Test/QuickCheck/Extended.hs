module Test.QuickCheck.Extended
    ( module Test.QuickCheck
    , shrinkBoundedEnum
    ) where

import Data.Maybe
    ( mapMaybe )
import Safe
    ( toEnumMay )

import Test.QuickCheck

shrinkBoundedEnum :: (Bounded a, Enum a) => a -> [a]
shrinkBoundedEnum = mapMaybe toEnumMay . shrink . fromEnum
