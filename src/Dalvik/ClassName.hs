{-# LANGUAGE OverloadedStrings #-}
module Dalvik.ClassName (
  ClassName,
  simpleClassName,
  qualifiedClassName,
  renderClassName,
  humanClassName
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import Data.Monoid

-- | An abstract representation of a Java class name.  These can be
-- rendered into mangled type expression format: e.g.,
--
-- > Ljava/lang/Object;
data ClassName = ClassName [BS.ByteString]
               deriving (Eq, Ord, Show)

instance Hashable ClassName where
  hashWithSalt s (ClassName cs) = hashWithSalt s cs

simpleClassName :: BS.ByteString -> ClassName
simpleClassName name = ClassName [name]

qualifiedClassName :: [BS.ByteString] -> BS.ByteString -> ClassName
qualifiedClassName namespace name = ClassName (namespace ++ [name])

renderClassName :: ClassName -> BS.ByteString
renderClassName (ClassName components) =
  mconcat [ "L", BS.intercalate "/" components, ";" ]

humanClassName :: ClassName -> String
humanClassName (ClassName components) =
  BS.unpack $ BS.intercalate "." components

