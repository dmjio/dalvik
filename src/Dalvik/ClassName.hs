{-# LANGUAGE OverloadedStrings #-}
module Dalvik.ClassName (
  ClassName,
  simpleClassName,
  qualifiedClassName,
  unsafeClassName,
  renderClassName,
  humanClassName,
  mapClassName
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Monoid
import qualified Data.Serialize as S

-- | An abstract representation of a Java class name.  These can be
-- rendered into mangled type expression format: e.g.,
--
-- > Ljava/lang/Object;
data ClassName = ClassName [BS.ByteString]
               deriving (Eq, Ord, Read, Show)

instance S.Serialize ClassName where
  put (ClassName cs) = S.put cs
  get = ClassName <$> S.get

instance Hashable ClassName where
  hashWithSalt s (ClassName cs) = hashWithSalt s cs

simpleClassName :: BS.ByteString -> ClassName
simpleClassName name = ClassName [name]

qualifiedClassName :: [BS.ByteString] -> BS.ByteString -> ClassName
qualifiedClassName namespace name = ClassName (namespace ++ [name])

-- | Create a 'ClassName' without verifying that the list is non-empty.
unsafeClassName :: [BS.ByteString] -> ClassName
unsafeClassName = ClassName

renderClassName :: ClassName -> BS.ByteString
renderClassName (ClassName components) =
  mconcat [ "L", BS.intercalate "/" components, ";" ]

humanClassName :: ClassName -> String
humanClassName (ClassName components) =
  BS.unpack $ BS.intercalate "." components

mapClassName :: ([BS.ByteString] -> [BS.ByteString]) -> ClassName -> ClassName
mapClassName f (ClassName components) = ClassName (f components)
