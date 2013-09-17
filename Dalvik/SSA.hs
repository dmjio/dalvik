{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Dalvik.SSA where

import Control.Failure
import qualified Data.ByteString as BS
import Data.Word (Word16)

import Dalvik.Instruction as I
import Dalvik.Types as DT
import Dalvik.SSA.Labelling
import Dalvik.SSA.Types as SSA

toSSA :: DT.DexFile -> SSA.DexFile
toSSA f = undefined -- dexClasses f

translateClass :: DT.Class -> SSA.Class
translateClass c =
  SSA.Class { --  classId = 0
            -- ,
              className = undefined
            , classParent = undefined
--            , classInterfaces = undefined
            -- , classStaticFields = undefined
            -- , classInstanceFields = undefined
            -- , classDirectMethods = undefined
            -- , classVirtualMethods = undefined
            }

labelMethod :: (Failure DecodeError f) => DT.DexFile -> DT.EncodedMethod -> f Labelling
labelMethod _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
labelMethod dx em@(DT.EncodedMethod _ _ (Just codeItem)) = do
  insts <- I.decodeInstructions (codeInsns codeItem)
  regMap <- methodRegisterAssignment dx em
  return $ labelInstructions regMap insts



methodExceptionRanges :: DT.DexFile -> EncodedMethod -> [ExceptionRange]
methodExceptionRanges _ _ = []

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
getParamList :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe BS.ByteString, BS.ByteString)]
getParamList df meth | isStatic meth = explicitParams df meth
                     | otherwise     = do
                        DT.Method cid _ _ <- getMethod df $ methId meth
                        clName <- getTypeName df cid
                        exParams <- explicitParams df meth
                        return ((Just "this", clName):exParams)
  where
    explicitParams dexFile (DT.EncodedMethod mId _ _) = do
      DT.Method _ pid _ <- getMethod dexFile mId
      DT.Proto  _   _ paramIDs <- getProto df pid

      -- This mapM will result in Nothing if /any/ of the types are
      -- unavailable, that's going to make debugging tricky.
      params <- mapM (getTypeName dexFile) paramIDs
      return $ findNames params

    findNames :: [BS.ByteString] -> [(Maybe BS.ByteString, BS.ByteString)]
    findNames ps = map (\p -> (Nothing, p)) ps

-- | Map argument names for a method to the initial register for that
-- argument.
--
methodRegisterAssignment :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe BS.ByteString, Word16)]
methodRegisterAssignment _  (DT.EncodedMethod mId _ Nothing)     = failure $ NoCodeForMethod mId
methodRegisterAssignment df meth@(DT.EncodedMethod _ _ (Just code)) = do
  params <- getParamList df meth
  return $ reverse $ snd $ accumOffsets params
    where
      accumOffsets params = foldr findOffset (codeRegs code, []) params

      findOffset :: (Maybe BS.ByteString, BS.ByteString) ->
                    (Word16, [(Maybe BS.ByteString, Word16)]) ->
                    (Word16, [(Maybe BS.ByteString, Word16)])
      findOffset (mName, tname) (offset, acc) = let
        regCount = registers tname
        in (offset - regCount, acc ++ [(mName, offset - regCount)])

      registers :: BS.ByteString -> Word16
      registers name | name == "J" = 2 -- longs take two registers.
                     | name == "D" = 2 -- doubles take two registers.
                     | otherwise   = 1 -- everything else fits in one.



{- Note [Translation]

Before building up the SSA-based IR, we label every Value with its
local SSA number using the algorithm from

  http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf

This is fairly different from the Cytron algorithm.  It works
backwards instead of forwards and does not require a dominance
frontier or a full CFG.  Once each value is identified this way,
making an SSA value for it should be simpler.

-}
