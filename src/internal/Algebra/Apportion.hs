{- HLINT ignore "Use camelCase" -}

module Algebra.Apportion
    where

import Data.Bifunctor
    ( bimap )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( mapMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Null
    ( MonoidNull )
import Data.MonoidMap
    ( MonoidMap )
import Data.Proxy
    ( Proxy )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Property
    , Testable
    , arbitrary
    , checkCoverage
    , forAllShrink
    , property
    , shrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Algebra.Apportion.Natural as Natural
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Apportion a where

    apportion
        :: a -> NonEmpty a -> (a, NonEmpty a)
    apportionMaybe
        :: a -> NonEmpty a -> Maybe (NonEmpty a)

    default apportion
        :: Monoid a
        => a -> NonEmpty a -> (a, NonEmpty a)
    apportion a as = case apportionMaybe a as of
        Nothing -> (a, mempty <$ as)
        Just bs -> (mempty, bs)

    default apportionMaybe
        :: (Eq a, Monoid a)
        => a -> NonEmpty a -> Maybe (NonEmpty a)
    apportionMaybe a as = case apportion a as of
       (b, bs) | b == mempty -> Just bs
       (_, _) -> Nothing

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

apportionLaw_length :: Apportion a => a -> NonEmpty a -> Bool
apportionLaw_length a as =
    length (snd (apportion a as)) == length as

apportionLaw_sum :: (Eq a, Monoid a, Apportion a) => a -> NonEmpty a -> Bool
apportionLaw_sum a as =
    F.fold (uncurry NE.cons (apportion a as)) == a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Apportion (Sum Natural) where
    apportionMaybe a as =
        fmap Sum <$> Natural.apportion (getSum a) (getSum <$> as)

deriving via Sum Natural instance Apportion Natural

instance (Ord k, Apportion v, Eq v, MonoidNull v) => Apportion (MonoidMap k v)
  where
    apportion m ms
        = F.foldl' combine empty $ apportionForKey <$> F.toList allKeys
      where
        allKeys :: Set k
        allKeys = F.foldMap MonoidMap.nonNullKeys (m : F.toList ms)

        combine
            :: (MonoidMap k v, NonEmpty (MonoidMap k v))
            -> (MonoidMap k v, NonEmpty (MonoidMap k v))
            -> (MonoidMap k v, NonEmpty (MonoidMap k v))
        combine (v0, vs0) (v1, vs1) = (v0 <> v1, NE.zipWith (<>) vs0 vs1)

        empty :: (MonoidMap k v, NonEmpty (MonoidMap k v))
        empty = (mempty, mempty <$ ms)

        apportionForKey :: k -> (MonoidMap k v, NonEmpty (MonoidMap k v))
        apportionForKey k
            = bimap
                (MonoidMap.singleton k)
                (fmap (MonoidMap.singleton k))
            $ apportion (MonoidMap.get k m) (MonoidMap.get k <$> ms)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

apportionLaws
    :: forall a.
        ( Arbitrary a
        , Eq a
        , Monoid a
        , Apportion a
        , Show a
        )
    => Proxy a
    -> Laws
apportionLaws _ = Laws "Apportion"
    [ ( "Length"
      , makeProperty apportionLaw_length)
    , ( "Sum"
      , makeProperty apportionLaw_sum)
    ]
  where
    makeProperty :: (a -> NonEmpty a -> Bool) -> Property
    makeProperty =
        property . forAllShrink genWeights shrinkWeights . makePropertyInner
      where
        genWeights :: Gen (NonEmpty a)
        genWeights = (:|) <$> arbitrary <*> arbitrary

        shrinkWeights :: NonEmpty a -> [NonEmpty a]
        shrinkWeights = mapMaybe NE.nonEmpty . shrink . NE.toList

    makePropertyInner
        :: (a -> NonEmpty a -> Bool)
        -> NonEmpty a
        -> (a -> Property)
    makePropertyInner condition weights value =
        checkCoverage $
        buildCoverage value weights result $
        condition value weights
      where
        result = apportion value weights

    buildCoverage
        :: Testable prop
        => a
        -> NonEmpty a
        -> (a, NonEmpty a)
        -> prop
        -> Property
    buildCoverage _value _weights _result = property
