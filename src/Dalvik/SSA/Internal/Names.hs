{-# LANGUAGE FlexibleContexts #-}
module Dalvik.SSA.Internal.Names (
  parseTypeName
  ) where

import Control.Failure
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 ( ByteString )
import Dalvik.ClassHierarchy
import Dalvik.SSA.Types
import Dalvik.Types ( DecodeError(..) )

parseTypeName :: (Failure DecodeError f) => ByteString -> f Type
parseTypeName bs =
  case parseOnly p bs of
    Left err -> failure $ TypeDecodeError err bs
    Right t -> return t

primType :: Type -> Parser Type
primType t = endOfInput >> return t

p :: Parser Type
p = do
  c1 <- anyChar
  case c1 of
    'V' -> primType VoidType
    'Z' -> primType BooleanType
    'B' -> primType ByteType
    'S' -> primType ShortType
    'C' -> primType CharType
    'I' -> primType IntType
    'J' -> primType LongType
    'F' -> primType FloatType
    'D' -> primType DoubleType
    '[' -> do
      t' <- p
      return $ ArrayType t'
    'L' -> do
      cs <- takeWhile1 (notInClass "/;") `sepBy1` char '/'
      let c:rcomps = reverse cs
      _ <- char ';'
      endOfInput
      return $ ReferenceType (qualifiedClassName (reverse rcomps) c)
    _ -> fail "Not a valid Java type name"
