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

-- | Map argument names for a method to the initial register for that
-- argument.
--
-- Note: argument names are available in the DebugInfo of CodeItem
methodRegisterAssignment :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe String, Word16)]
methodRegisterAssignment _  (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
methodRegisterAssignment df (DT.EncodedMethod mId _ (Just code)) = do
  DT.Method cid pid nameId <- getMethod df mId
  DT.Proto    _   _ params <- getProto df pid

  -- This mapM will result in Nothing if /any/ of the types are
  -- unavailable, that's going to make debugging tricky.
  paramNames <- mapM (getTypeName df) params
  return $ snd $ foldr findOffset (codeRegs code, []) (reverse paramNames)
    where
      findOffset :: BS.ByteString ->
                    (Word16, [(Maybe String, Word16)]) ->
                    (Word16, [(Maybe String, Word16)])
      findOffset tname (offset, acc) = let
        regCount = registers tname
        in (offset - regCount, acc ++ [(Nothing, offset)])

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
