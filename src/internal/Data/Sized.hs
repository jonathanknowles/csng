{-# LANGUAGE UndecidableInstances #-}

module Data.Sized where

import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.MonoidMap
    ( MonoidMap )
import Data.Strict.Map
    ( Map )
import Data.Strict.Set
    ( Set )
import Numeric.Natural
    ( Natural )

class Sized a where
    type Size a
    size :: a -> Size a

newtype FoldableSized (f :: Type -> Type) a = FoldableSized (f a)

deriving instance Foldable f => Foldable (FoldableSized f)

instance Foldable f => Sized (FoldableSized f a) where
    type Size (FoldableSized f a) = Natural
    size = fromIntegral @Int @Natural . length

deriving via FoldableSized [] a instance Sized [a]
deriving via FoldableSized NonEmpty a instance Sized (NonEmpty a)
deriving via FoldableSized (Map k) a instance Sized (Map k a)
deriving via FoldableSized (MonoidMap k) a instance Sized (MonoidMap k a)
deriving via FoldableSized Set a instance Sized (Set a)
