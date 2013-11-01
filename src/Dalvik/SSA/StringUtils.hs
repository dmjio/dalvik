module Dalvik.SSA.StringUtils where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.List.Split
import qualified Data.Set as S

import Dalvik.SSA.ClassHierarchy
import Dalvik.SSA.Types

--------------------------------------------------------------------------------
-- String utilities for naming types and methods

type MethodNamePattern = (Type, ClassName, ByteString, [Type])

-- | Given a 'MethodNamePattern' @mnp@ and a 'MethodRef' @mref@,
-- returns whether @mnp@ matchs @mref@. The semantics of matching here
-- include subtyping, so if @mnp@ refers to a superclass of @mref@, it
-- will still match.
refMatches :: ClassHierarchy
           -> MethodNamePattern
           -> MethodRef
           -> Bool
refMatches cha (ret, clName, methName, params) = matchAny
  where
    matchAny ref = S.null . S.filter (match ref) $ candidates
    match ref meth = and [ methodRefClass ref == (classType . methodClass $ meth)
                         , methodRefName  ref == methName
                         , methodRefReturnType ref == methodReturnType meth
                         , methodRefParameterTypes ref == (map parameterType . methodParameters $ meth)
                    ]
    fakeRef = MethodRef 0 (ReferenceType clName) ret params methName
    candidates = S.fromList $ implementationsOf cha clName fakeRef

-- | Return all 'Method's matching the 'MethodNamePattern' in the
-- current class hierarchy.
allMatches :: ClassHierarchy
           -> MethodNamePattern
           -> [Method]
allMatches cha (ret, clName, methName, params) = implementationsOf cha clName fakeRef
  where fakeRef = MethodRef 0 (ReferenceType clName) ret params methName

-- | Build a 'MethodNamePattern' from a fully-qualified Java method
-- name.
patFromString :: String -> Maybe MethodNamePattern
patFromString str = do
  (retStr:methStr') <- return $ words str
  let methStr = unwords methStr'
  ret <- typeFromString retStr
  let (methQName, params) = span (/= '(') methStr
  (pkgs, clName, methName) <-
    case map BS.pack (splitOn "." methQName) of
      xs@(_:_:_) -> return (init (init xs), last (init xs), last xs)
      _          -> Nothing
  -- drop parens and split on commas, with spaces OK
  let paramSplit = filter (not . null)
                 . split (condense . dropDelims $ oneOf ", ")
                 . filter (not . (`elem` "()"))
                 $ params
  paramTys <- mapM typeFromString paramSplit
  return (ret, qualifiedClassName pkgs clName, methName, paramTys)

-- | Get a 'Type' from a string representing a fully-qualified Java
-- class name.
typeFromString :: String -> Maybe Type
typeFromString ""  = Nothing
typeFromString str =  arrayTypeFromString str
                  <|> primTypeFromString str
                  <|> refTypeFromString str

arrayTypeFromString :: String -> Maybe Type
arrayTypeFromString str = do
  guard ("[]" `L.isSuffixOf` str)
  let contents = reverse . drop 2 . reverse $ str
  ArrayType <$> typeFromString contents

primTypeFromString :: String -> Maybe Type
primTypeFromString str =
  case str of
    "void"    -> return VoidType
    "byte"    -> return ByteType
    "short"   -> return ShortType
    "int"     -> return IntType
    "long"    -> return LongType
    "float"   -> return FloatType
    "double"  -> return DoubleType
    "char"    -> return CharType
    "boolean" -> return BooleanType
    _         -> Nothing

refTypeFromString :: String -> Maybe Type
refTypeFromString str = do
  let (package, clName) =
        case map BS.pack (splitOn "." str) of
          [name] -> ([], name)
          xs     -> (init xs, last xs)
  guard (not (BS.null clName) && isUpper (BS.head clName))
  return . ReferenceType $ qualifiedClassName package clName
