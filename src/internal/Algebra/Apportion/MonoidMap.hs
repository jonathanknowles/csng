{-# OPTIONS_GHC -fno-warn-orphans #-}

module Algebra.Apportion.MonoidMap
    ()
    where

import Algebra.Apportion
    ( Apportion (..), Apportionment (..), ExactApportion, apportionList )
import Data.Monoid.Monus.Extended
    ()
import Data.MonoidMap
    ( MonoidMap )
import Data.Semialign
    ( salign )
import Data.Set
    ( Set )
import Test.QuickCheck.Instances.NonEmpty
    ()

import qualified Data.Foldable as F
import qualified Data.MonoidMap as MonoidMap

instance (Ord k, Apportion v, Weight v ~ v) => Apportion (MonoidMap k v)
  where
    type Weight (MonoidMap k v) = MonoidMap k v
    apportion = apportionList apportionInner
      where
        apportionInner m ms =
            F.foldl' salign empty $ apportionForKey <$> F.toList allKeys
          where
            allKeys :: Set k
            allKeys = F.foldMap MonoidMap.keys (m : F.toList ms)

            empty :: Apportionment [] (MonoidMap k v)
            empty = Apportionment mempty (mempty <$ F.toList ms)

            apportionForKey :: k -> Apportionment [] (MonoidMap k v)
            apportionForKey k = MonoidMap.singleton k <$>
                apportion (MonoidMap.get k m) (MonoidMap.get k <$> F.toList ms)

instance (Ord k, Apportion v, Weight v ~ v) => ExactApportion (MonoidMap k v)
