{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Dalvik.SSA where

import Control.Failure
import Control.Monad ( liftM )
import qualified Data.ByteString as BS
import qualified Data.Vector as V
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
  ers <- methodExceptionRanges dx em
  return $ labelInstructions regMap insts

methodExceptionRanges :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [ExceptionRange]
methodExceptionRanges _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
methodExceptionRanges dx (DT.EncodedMethod mId _ (Just codeItem)) = do
  mapM toExceptionRange (codeTryItems codeItem)
  where
    catches = V.fromList $ codeHandlers codeItem
    toExceptionRange tryItem = do
      let handlerOff = tryHandlerOff tryItem
      -- The fromIntegral here is not lossy: Word16 to Int
      case catches V.!? fromIntegral handlerOff of
        Nothing -> failure $ NoHandlerAtIndex handlerOff
        Just ch -> do
          typeNames <- mapM (\(tix, off) -> liftM (, off) (getTypeName dx tix)) (chHandlers ch)
          return ExceptionRange { erOffset = tryStartAddr tryItem
                                , erCount = tryInsnCount tryItem
                                , erCatch = undefined
                                , erCatchAll = chAllAddr ch
                                }

-- | Map argument names for a method to the initial register for that
-- argument.
--
-- Note: argument names are available in the DebugInfo of CodeItem
methodRegisterAssignment :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe String, Word16)]
methodRegisterAssignment _  (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
methodRegisterAssignment df (DT.EncodedMethod mId _ (Just code)) = do
  DT.Method cid pid nameId <- getMethod df mId
  DT.Proto    _   _ params <- getProto df pid

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
