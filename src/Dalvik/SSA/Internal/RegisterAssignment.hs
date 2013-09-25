{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Dalvik.SSA.Internal.RegisterAssignment (
  getParamList,
  getParamListTypeNames,
  methodRegisterAssignment
  ) where

import Control.Arrow ( first )
import Control.Failure
import Control.Monad ( forM )
import qualified Data.ByteString as BS
import Data.Word ( Word16 )

import Dalvik.Types as DT

getParamList :: (Failure DecodeError f)
                => DT.DexFile
                -> EncodedMethod
                -> f [(Maybe BS.ByteString, DT.TypeId)]
getParamList df meth
  | isStatic meth = explicitParams df meth
  | otherwise     = do
    DT.Method cid _ _ <- getMethod df $ methId meth
    exParams <- explicitParams df meth
    return ((Just "this", cid):exParams)
  where
    explicitParams dexFile (DT.EncodedMethod mId _ _) = do
      DT.Method _ pid _ <- getMethod dexFile mId
      DT.Proto  _   _ paramIDs <- getProto df pid

      return $ findNames (methCode meth) paramIDs

    findNames :: Maybe CodeItem -> [DT.TypeId] -> [(Maybe BS.ByteString, DT.TypeId)]
    findNames (Just (CodeItem { codeDebugInfo = Just di })) ps =
      map (first attachParamName) psWithNameIndices
      where
        psWithNameIndices = zip (dbgParamNames di) ps
        attachParamName ix
          | Just name <- getStr df (fromIntegral ix) = Just name
          | otherwise = Nothing
    findNames _ ps = map (\p -> (Nothing, p)) ps

-- | extract the parameter list from an encoded method.  This returns
-- a list of `(Maybe name, typeName)` pairs, in left-to-right order,
-- and including the initial `this` parameter, if the method is an
-- instance method (non-static)
--
-- For example, when given the following method:
-- > public Object stringID(String s) {
-- >        return s;
-- > }
--
-- this method would return:
-- > Just [(Just "this","LTest;"), (Nothing, "Ljava/lang/String;")]
getParamListTypeNames :: (Failure DecodeError f)
                         => DT.DexFile
                         -> EncodedMethod
                         -> f [(Maybe BS.ByteString, BS.ByteString)]
getParamListTypeNames df meth = do
  plist <- getParamList df meth
  forM plist $ \(n, tid) -> do
    tname <- getTypeName df tid
    return (n, tname)


-- | Map argument names for a method to the initial register for that
-- argument.
--
methodRegisterAssignment :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe BS.ByteString, Word16)]
methodRegisterAssignment _  (DT.EncodedMethod mId _ Nothing)     = failure $ NoCodeForMethod mId
methodRegisterAssignment df meth@(DT.EncodedMethod _ _ (Just code)) = do
  params <- getParamListTypeNames df meth
  return $ snd $ accumOffsets params
    where
      accumOffsets params = foldr findOffset (codeRegs code, []) params

      findOffset :: (Maybe BS.ByteString, BS.ByteString) ->
                    (Word16, [(Maybe BS.ByteString, Word16)]) ->
                    (Word16, [(Maybe BS.ByteString, Word16)])
      findOffset (mName, tname) (offset, acc) = let
        regCount = registers tname
        in (offset - regCount, (mName, offset - regCount):acc)

      registers :: BS.ByteString -> Word16
      registers name | name == "J" = 2 -- longs take two registers.
                     | name == "D" = 2 -- doubles take two registers.
                     | otherwise   = 1 -- everything else fits in one.
