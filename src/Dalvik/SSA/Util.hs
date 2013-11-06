{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Dalvik.SSA.Util (
  findClassByName,
  findMethodByName,
  findStaticFieldByName,
  stripCasts,
  LookupError(..)
  ) where

import Control.Exception
import Control.Failure
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Maybe ( fromMaybe )
import Data.Typeable

import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Types

-- | Strip any cast instructions off of the given 'Value'
stripCasts :: (IsValue a) => a -> Value
stripCasts (toValue -> v) = fromMaybe v $ do
  CheckCast { castReference = r } <- fromValue v
  return (stripCasts r)

data LookupError = NoClassFound BS.ByteString
                 | NoMethodFound BS.ByteString String BS.ByteString
                 | NoFieldFound BS.ByteString BS.ByteString
                 deriving (Eq, Ord, Show, Typeable)

instance Exception LookupError

findClassByName :: (Failure LookupError f) => BS.ByteString -> DexFile -> f Class
findClassByName name df = maybe err return $ do
  L.find ((== name) . className) (dexClasses df)
  where
    err = failure $ NoClassFound name

findMethodByName :: (Failure LookupError f) => BS.ByteString -> String -> Class -> f Method
findMethodByName mname sig klass = maybe err return $ do
  tsig <- parseMethodSignature (BS.pack sig)
  L.find (matchingMethod tsig) ms
  where
    err = failure $ NoMethodFound mname sig (className klass)
    ms = classDirectMethods klass ++ classVirtualMethods klass
    matchingMethod tsig m =
      let ps = if methodIsVirtual m then tail (methodParameters m) else methodParameters m
          msig = (map parameterType ps, methodReturnType m)
      in methodName m == mname && msig == tsig

findStaticFieldByName :: (Failure LookupError f) => BS.ByteString -> Class -> f Field
findStaticFieldByName name klass = maybe err (return . snd) $ do
  L.find ((== name) . fieldName . snd) (classStaticFields klass)
  where
    err = failure $ NoFieldFound name (className klass)
