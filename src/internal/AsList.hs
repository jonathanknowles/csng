{-# LANGUAGE UndecidableInstances #-}

module AsList where

import GHC.Exts
    ( IsList (..) )
import Text.Read
    ( Read (..) )

newtype AsList a = AsList {unAsList :: a}

instance (IsList a, Show (Item a)) => Show (AsList a) where
    show = show . toList . unAsList

instance (IsList a, Read (Item a)) => Read (AsList a) where
    readPrec = AsList . fromList <$> readPrec

asList :: (IsList a, IsList b) => ([Item a] -> [Item b]) -> a -> b
asList f = fromList . f . toList
