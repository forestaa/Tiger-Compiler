{-# LANGUAGE UndecidableInstances #-}

module Compiler.Utils.Maybe where

import GHC.Records (HasField (..))
import RIO

instance (HasField field x a) => HasField field (Maybe x) (Maybe a) where
  getField = fmap (getField @field)

infixl 5 ?.

(?.) :: forall field x a. (HasField field x (Maybe a)) => Maybe x -> Proxy field -> Maybe a
m ?. _ = m >>= getField @field
