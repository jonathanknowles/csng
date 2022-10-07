module ValueSpec where

import Prelude

import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck
    ( Arbitrary )
import Test.QuickCheck.Classes
    ( isListLaws
    , monoidLaws
    , semigroupLaws
    , semigroupMonoidLaws
    , showLaws
    , showReadLaws
    )
import Test.QuickCheck.Classes.Group
    ( groupLaws )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Monoid.GCD
    ( overlappingGCDMonoidLaws )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws, positiveMonoidLaws )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Quid
    ( Latin (..), Quid )
import Value
    ( Balance, Coin )

spec :: Spec
spec = do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @(Balance TestAsset)
            [ commutativeLaws
            , groupLaws
            , isListLaws
            , monoidLaws
            , monoidNullLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showLaws
            , showReadLaws
            ]
        testLawsMany @(Coin TestAsset)
            [ cancellativeLaws
            , commutativeLaws
            , isListLaws
            , leftCancellativeLaws
            , leftReductiveLaws
            , monoidLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , positiveMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showLaws
            , showReadLaws
            ]

newtype TestAsset = TestAsset (Latin Quid)
    deriving stock (Eq, Ord, Read, Show)
    deriving Arbitrary via Quid
