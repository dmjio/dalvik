{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dalvik.SSA where

import Control.Arrow ( first )
import Control.Failure
import Control.Monad ( foldM, liftM )
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as BS
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Word (Word16)

import Dalvik.Instruction as I
import Dalvik.Types as DT
import Dalvik.SSA.Labeling
import Dalvik.SSA.Types as SSA
import Dalvik.SSA.Internal.Names

toSSA :: (MonadFix f, Failure DecodeError f) => DT.DexFile -> f SSA.DexFile
toSSA df = do
  -- FIXME: Might want to put references to the appropriate class
  -- in ReferenceType - then we would need to add the type map into
  -- the knot tying process
  dexIdentifierBS <- getStr df (dexThisId df)
  typeMap <- foldM translateType M.empty $ M.toList (DT.dexTypeNames df)
  cmap <- mfix $ \cmap' -> do
    foldM (translateClass df typeMap cmap') M.empty $ M.toList (DT.dexClasses df)
  return SSA.DexFile { dexIdentifier = BS.unpack dexIdentifierBS
                     , SSA.dexClasses = M.elems cmap
                     }
  where
    translateType :: (Failure DecodeError f)
                     => Map DT.TypeId SSA.Type
                     -> (DT.TypeId, DT.StringId)
                     -> f (Map DT.TypeId SSA.Type)
    translateType m (tid, _) = do
      tname <- getTypeName df tid
      ty <- parseTypeName tname
      return $ M.insert tid ty m

-- FIXME: We might need maps here for methods and fields, too, since
-- we need direct access to those things.
translateClass :: (Failure DecodeError f)
                  => DT.DexFile
                  -> Map DT.TypeId SSA.Type
                  -> Map DT.TypeId SSA.Class
                  -> Map DT.TypeId SSA.Class
                  -> (DT.TypeId, DT.Class)
                  -> f (Map DT.TypeId SSA.Class)
translateClass = undefined
{-
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
-}

labelMethod :: (Failure DecodeError f) => DT.DexFile -> DT.EncodedMethod -> f Labeling
labelMethod _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
labelMethod dx em@(DT.EncodedMethod _ _ (Just codeItem)) = do
  insts <- I.decodeInstructions (codeInsns codeItem)
  regMap <- methodRegisterAssignment dx em
  ers <- methodExceptionRanges dx em
  return $ labelInstructions regMap ers insts

-- | Parse the try/catch description tables for this 'EncodedMethod'
-- from the DexFile.  The tables are reduced to summaries
-- ('ExceptionRange') that are easier to work with.
methodExceptionRanges :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [ExceptionRange]
methodExceptionRanges _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
methodExceptionRanges dx (DT.EncodedMethod _ _ (Just codeItem)) = do
  mapM toExceptionRange (codeTryItems codeItem)
  where
    catches = V.fromList $ codeHandlers codeItem
    toExceptionRange tryItem = do
      let hOffset = fromIntegral $ tryHandlerOff tryItem
      case V.findIndex ((==hOffset) . chHandlerOff) catches of
        Nothing ->failure $ NoHandlerAtOffset hOffset
        Just cix -> do
          let Just ch = catches V.!? cix
          typeNames <- mapM (\(tix, off) -> liftM (, off) (getTypeName dx tix)) (chHandlers ch)
          return ExceptionRange { erOffset = tryStartAddr tryItem
                                , erCount = tryInsnCount tryItem
                                , erCatch = typeNames
                                , erCatchAll = chAllAddr ch
                                }

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
getParamList :: (Failure DecodeError f)
                => DT.DexFile
                -> EncodedMethod
                -> f [(Maybe BS.ByteString, BS.ByteString)]
getParamList df meth
  | isStatic meth = explicitParams df meth
  | otherwise     = do
    DT.Method cid _ _ <- getMethod df $ methId meth
    clName <- getTypeName df cid
    exParams <- explicitParams df meth
    return ((Just "this", clName):exParams)
  where
    explicitParams dexFile (DT.EncodedMethod mId _ _) = do
      DT.Method _ pid _ <- getMethod dexFile mId
      DT.Proto  _   _ paramIDs <- getProto df pid

      params <- mapM (getTypeName dexFile) paramIDs
      return $ findNames (methCode meth) params

    findNames :: Maybe CodeItem -> [BS.ByteString] -> [(Maybe BS.ByteString, BS.ByteString)]
    findNames (Just (CodeItem { codeDebugInfo = Just di })) ps =
      map (first attachParamName) psWithNameIndices
      where
        psWithNameIndices = zip (dbgParamNames di) ps
        attachParamName ix
          | Just name <- getStr df (fromIntegral ix) = Just name
          | otherwise = Nothing
    findNames _ ps = map (\p -> (Nothing, p)) ps


-- | Map argument names for a method to the initial register for that
-- argument.
--
methodRegisterAssignment :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [(Maybe BS.ByteString, Word16)]
methodRegisterAssignment _  (DT.EncodedMethod mId _ Nothing)     = failure $ NoCodeForMethod mId
methodRegisterAssignment df meth@(DT.EncodedMethod _ _ (Just code)) = do
  params <- getParamList df meth
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



{- Note [Translation]

Before building up the SSA-based IR, we label every Value with its
local SSA number using the algorithm from

  http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf

This is fairly different from the Cytron algorithm.  It works
backwards instead of forwards and does not require a dominance
frontier or a full CFG.  Once each value is identified this way,
making an SSA value for it should be simpler.

-}
