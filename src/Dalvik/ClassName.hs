{-# LANGUAGE OverloadedStrings #-}
module Dalvik.ClassName (
  ClassName,
  simpleClassName,
  qualifiedClassName,
  unsafeClassName,
  renderClassName,
  humanClassName
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Monoid
import qualified Text.Show.Pretty as PP

-- | An abstract representation of a Java class name.  These can be
-- rendered into mangled type expression format: e.g.,
--
-- > Ljava/lang/Object;
data ClassName = ClassName [BS.ByteString]
               deriving (Eq, Ord, Read, Show)

instance PP.PrettyVal ClassName where
  prettyVal = PP.prettyVal . humanClassName

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

