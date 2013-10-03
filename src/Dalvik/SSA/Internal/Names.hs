{-# LANGUAGE FlexibleContexts #-}
module Dalvik.SSA.Internal.Names (
  generateNameForParameter,
  parseTypeName,
  parseMethodSignature
  ) where

import Control.Applicative
import Control.Failure
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 ( ByteString )
import Data.Monoid
import Data.String ( IsString, fromString )
import Dalvik.ClassHierarchy
import Dalvik.SSA.Types
import Dalvik.Types ( DecodeError(..) )

-- | Create a new name for an unnamed parameter.  Parameters do not
-- have names in the bytecode if the dex file was compiled without
-- debug information.
generateNameForParameter :: (Monoid a, IsString a) => Int -> a
generateNameForParameter ix = fromString "%arg" <> fromString (show ix)

-- | Parse a raw Dalvik type descriptor string into a structured 'Type'
parseTypeName :: (Failure DecodeError f) => ByteString -> f Type
parseTypeName bs =
  case parseOnly p bs of
    Left err -> failure $ TypeDecodeError err bs
    Right t -> return t

-- | Parse Java method type signatures of the form:
--
-- > (t1t2)rt
parseMethodSignature :: (Failure DecodeError f) => ByteString -> f ([Type], Type)
parseMethodSignature bs =
  case parseOnly pSig bs of
    Left err -> failure $ TypeDecodeError err bs
    Right sig -> return sig

primType :: Type -> Parser Type
primType t = endOfInput >> return t

pSig :: Parser ([Type], Type)
pSig = do
  _ <- char '('
  ptypes <- many p
  _ <- char ')'
  rt <- p
  endOfInput
  return (ptypes, rt)

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
