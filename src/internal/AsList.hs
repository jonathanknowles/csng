{-# LANGUAGE UndecidableInstances #-}

module AsList where

import GHC.Exts
    ( IsList (..) )
import Text.Read
    ( Read (..) )

newtype AsList a = AsList {asList :: a}

instance (IsList a, Show (Item a)) => Show (AsList a) where
    show = show . toList . asList

instance (IsList a, Read (Item a)) => Read (AsList a) where
    readPrec = AsList . fromList <$> readPrec
