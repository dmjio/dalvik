module Dalvik.SSA.Internal.Names (
  encodeType,
  generateNameForParameter,
  parseTypeName,
  parseMethodSignature
  ) where

import Control.Applicative
import qualified Control.Monad.Catch as E
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.Builder as LBS
import Data.Monoid
import Data.String ( IsString, fromString )
import Dalvik.SSA.Types
import Dalvik.Types ( DecodeError(..) )

-- | Create a new name for an unnamed parameter.  Parameters do not
-- have names in the bytecode if the dex file was compiled without
-- debug information.
generateNameForParameter :: (Monoid a, IsString a) => Int -> a
generateNameForParameter ix = fromString "%arg" <> fromString (show ix)

-- | Parse a raw Dalvik type descriptor string into a structured 'Type'
parseTypeName :: (E.MonadThrow m) => ByteString -> m Type
parseTypeName bs =
  case parseOnly p' bs of
    Left err -> E.throwM $ TypeDecodeError err bs
    Right t -> return t
  where
    p' = do
      t <- p
      endOfInput
      return t

-- | Parse Java method type signatures of the form:
--
-- > (t1t2)rt
parseMethodSignature :: (E.MonadThrow m) => ByteString -> m ([Type], Type)
parseMethodSignature bs =
  case parseOnly pSig bs of
    Left err -> E.throwM $ TypeDecodeError err bs
    Right sig -> return sig

primType :: Type -> Parser Type
primType = return

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
      return $ ReferenceType (qualifiedClassName (reverse rcomps) c)
    'U' -> primType UnknownType
    _ -> fail "Not a valid Java type name"

encodeType :: Type -> ByteString
encodeType = LBS.toStrict . LBS.toLazyByteString . go
  where
    go t =
      case t of
        VoidType -> LBS.char8 'V'
        BooleanType -> LBS.char8 'Z'
        ByteType -> LBS.char8 'B'
        ShortType -> LBS.char8 'S'
        CharType -> LBS.char8 'C'
        IntType -> LBS.char8 'I'
        LongType -> LBS.char8 'J'
        FloatType -> LBS.char8 'F'
        DoubleType -> LBS.char8 'D'
        ArrayType t' -> LBS.char8 '[' `mappend` go t'
        ReferenceType n -> LBS.byteString (renderClassName n)
        UnknownType -> LBS.char8 'U'
