{-# LANGUAGE OverloadedLists #-}

module ValueSpec where

import Data.Group
    ( Group (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary, Property, property, (===) )
import Test.QuickCheck.Classes
    ( eqLaws
    , isListLaws
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
    ( Assets (..)
    , Balance
    , BalanceValue
    , Coin
    , CoinValue
    , FractionalCoin
    , FractionalCoinValue
    , Values (..)
    , balanceToCoins
    , coinToBalance
    )

spec :: Spec
spec = do

    parallel $ describe "Class instances obey laws" $ do
        testLawsMany @TestBalance
            [ commutativeLaws
            , eqLaws
            , groupLaws
            , isListLaws
            , monoidLaws
            , monoidNullLaws
            , semigroupLaws
            , semigroupMonoidLaws
            , showLaws
            , showReadLaws
            ]
        testLawsMany @TestCoin
            [ cancellativeLaws
            , commutativeLaws
            , eqLaws
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

    parallel $ describe "Conversions" $ do
        it "prop_coinToBalance_balanceToCoins" $
            prop_coinToBalance_balanceToCoins
                & property
        it "prop_coinToBalance_invert_balanceToCoins" $
            prop_coinToBalance_invert_balanceToCoins
                & property
        it "prop_balanceToCoins_coinToBalance_invert" $
            prop_balanceToCoins_coinToBalance_invert
                & property

prop_coinToBalance_balanceToCoins :: TestCoin -> Property
prop_coinToBalance_balanceToCoins c =
    balanceToCoins (coinToBalance c) === (mempty, c)

prop_coinToBalance_invert_balanceToCoins :: TestCoin -> Property
prop_coinToBalance_invert_balanceToCoins c =
    balanceToCoins (invert (coinToBalance c)) === (c, mempty)

prop_balanceToCoins_coinToBalance_invert :: TestBalance -> Property
prop_balanceToCoins_coinToBalance_invert b =
    invert (coinToBalance n) <> coinToBalance p === b
  where
    (n, p) = balanceToCoins b

type TestBalance = Balance TestAsset
type TestCoin = Coin TestAsset

deriving instance Arbitrary (Assets TestCoin)
deriving instance Arbitrary (Values TestCoin)

newtype TestAsset = TestAsset (Latin Quid)
    deriving stock (Eq, Ord, Read, Show)
    deriving Arbitrary via Quid

--------------------------------------------------------------------------------
-- Literal tests
--------------------------------------------------------------------------------

data LatinChar
    = A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Show)

testBalance :: Balance LatinChar
testBalance =
    [ (A, -2)
    , (B, -1)
    , (C,  1)
    , (D,  2)
    ]

testBalanceValues :: [BalanceValue]
testBalanceValues = [-2, -1, 0, 1, 2]

testCoin :: Coin LatinChar
testCoin =
    [ (A, 1)
    , (B, 2)
    , (C, 3)
    , (D, 4)
    ]

testCoinValues :: [CoinValue]
testCoinValues = [0, 1, 2]

testFractionalCoin :: FractionalCoin LatinChar
testFractionalCoin =
    [ (A, 1%2)
    , (B, 1  )
    , (C, 3%2)
    , (D, 2  )
    ]

testFractionalCoinValues :: [FractionalCoinValue]
testFractionalCoinValues = [0, 1%2, 1, 3%2, 2]
