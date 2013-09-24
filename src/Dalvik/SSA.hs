{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Dalvik.SSA where

import Control.Arrow ( first )
import Control.Failure
import Control.Monad ( foldM, forM, liftM )
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word (Word16)

import Dalvik.Instruction as DT
import Dalvik.Types as DT
import Dalvik.SSA.BasicBlocks
import Dalvik.SSA.Labeling
import Dalvik.SSA.Types as SSA
import Dalvik.SSA.Internal.Names

toSSA :: (MonadFix f, Failure DecodeError f) => DT.DexFile -> f SSA.DexFile
toSSA df = do
  -- FIXME: Might want to put references to the appropriate class
  -- in ReferenceType - then we would need to add the type map into
  -- the knot tying process
  dexIdentifierBS <- getStr df (dexThisId df)
  Knot { knotClasses = cmap } <- mfix $ \knot ->
    liftM fst $ evalRWST tieKnot knot (initialKnotState df)
  return SSA.DexFile { dexIdentifier = BS.unpack dexIdentifierBS
                     , SSA.dexClasses = M.elems cmap
                     }

tieKnot :: (MonadFix f, Failure DecodeError f) => KnotMonad f Knot
tieKnot = do
  df <- gets knotDexFile
  knot' <- foldM translateType emptyKnot $ M.toList (DT.dexTypeNames df)
  foldM translateClass knot' $ M.toList (DT.dexClasses df)

type KnotMonad f = RWST Knot () KnotState f

data Knot = Knot { knotClasses :: Map DT.TypeId SSA.Class
                 , knotMethods :: Map DT.MethodId SSA.Method
                 , knotFields :: Map DT.FieldId SSA.Field
                 , knotTypes :: Map DT.TypeId SSA.Type
                 }

emptyKnot :: Knot
emptyKnot  = Knot { knotClasses = M.empty
                  , knotMethods = M.empty
                  , knotFields = M.empty
                  , knotTypes = M.empty
                  }

data KnotState = KnotState { knotIdSrc :: Int
                           , knotDexFile :: DT.DexFile
                           }

initialKnotState :: DT.DexFile -> KnotState
initialKnotState = KnotState 0

-- | FIXME: Attach the Class to ReferenceTypes.  This will require some changes
-- to the test suite...
translateType :: (Failure DecodeError f)
                 => Knot
                 -> (DT.TypeId, DT.StringId)
                 -> KnotMonad f Knot
translateType m (tid, _) = do
  df <- gets knotDexFile
  tname <- getTypeName df tid
  ty <- parseTypeName tname
  return m { knotTypes = M.insert tid ty (knotTypes m) }

getStr' :: (Failure DecodeError f) => DT.StringId -> KnotMonad f String
getStr' sid = do
  df <- gets knotDexFile
  liftM BS.unpack $ lift $ getStr df sid

lookupClass :: (Failure DecodeError f)
               => DT.TypeId
               -> KnotMonad f (Maybe SSA.Class)
lookupClass tid = do
  klasses <- asks knotClasses
  return $ M.lookup tid klasses

getTranslatedClass :: (Failure DecodeError f)
                      => DT.TypeId
                      -> KnotMonad f SSA.Class
getTranslatedClass tid = do
  klass <- lookupClass tid
  maybe (error ("No class for type id: " ++ show tid)) return klass

translateClass :: (MonadFix f, Failure DecodeError f)
                  => Knot
                  -> (DT.TypeId, DT.Class)
                  -> KnotMonad f Knot
translateClass k (tid, klass) = do
  cid <- freshId
  cname <- getStr' $ classSourceNameId klass
  parent <- lookupClass (classSuperId klass)
  staticFields <- mapM translateField (DT.classStaticFields klass)
  instanceFields <- mapM translateField (DT.classInstanceFields klass)
  directMethods <- mapM translateMethod (DT.classDirectMethods klass)
  virtualMethods <- mapM translateMethod (DT.classVirtualMethods klass)
  let c = SSA.Class { SSA.classId = cid
                    , SSA.className = cname
                    , SSA.classParent = parent
                    , SSA.classStaticFields = staticFields
                    , SSA.classInstanceFields = instanceFields
                    , SSA.classDirectMethods = directMethods
                    , SSA.classVirtualMethods = virtualMethods
                    }

  return k { knotClasses = M.insert tid c (knotClasses k) }

getRawMethod' :: (Failure DecodeError f) => DT.MethodId -> KnotMonad f DT.Method
getRawMethod' mid = do
  df <- gets knotDexFile
  lift $ getMethod df mid

getRawProto' :: (Failure DecodeError f) => DT.ProtoId -> KnotMonad f DT.Proto
getRawProto' pid = do
  df <- gets knotDexFile
  lift $ getProto df pid

translateMethod :: (MonadFix f, Failure DecodeError f) => DT.EncodedMethod -> KnotMonad f SSA.Method
translateMethod em = do
  m <- getRawMethod' (DT.methId em)
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)

  df <- gets knotDexFile
  -- [(Maybe BS.ByteString, DT.TypeId)]
  paramList <- lift $ getParamList df em
  paramMap <- foldM makeParameter M.empty (zip [0..] paramList)

  (body, _) <- mfix $ \(_, labelMap) ->
    translateMethodBody df paramMap labelMap em

  return SSA.Method { SSA.methodId = fromIntegral (DT.methId em)
                    , SSA.methodName = mname
                    , SSA.methodReturnType = rt
                    , SSA.methodAccessFlags = DT.methAccessFlags em
                    , SSA.methodParameters = M.elems paramMap
                    , SSA.methodBody = body
                    }

-- | FIXME: Use the paramMap to populate the initial method knot
translateMethodBody :: (MonadFix f, Failure DecodeError f)
                       => DT.DexFile
                       -> Map Int Parameter
                       -> MethodKnot
                       -> DT.EncodedMethod
                       -> KnotMonad f (Maybe [BasicBlock], MethodKnot)
translateMethodBody _ _ _ DT.EncodedMethod { DT.methCode = Nothing } = return (Nothing, emptyMethodKnot)
translateMethodBody df paramMap labelMap em = do
  labeling <- lift $ labelMethod df em
  let bbs = labelingBasicBlocks labeling
      blockList = basicBlocksAsList bbs
  (bs, tiedMknot) <- foldM (translateBlock labeling labelMap) ([], emptyMethodKnot) blockList
  return (Just (reverse bs), tiedMknot)

data MethodKnot = MethodKnot { mknotValues :: Map Label SSA.Value
                             , mknotBlocks :: Map BlockNumber SSA.BasicBlock
                             }

emptyMethodKnot :: MethodKnot
emptyMethodKnot = MethodKnot M.empty M.empty

-- | To translate a BasicBlock, we first construct any (non-trivial)
-- phi nodes for the block.  Then translate each instruction.
--
-- Note that the phi nodes (if any) are all at the beginning of the
-- block in an arbitrary order.  Any analysis should process all of
-- the phi nodes for a single block at once.
translateBlock :: (Failure DecodeError f)
                  => Labeling
                  -> MethodKnot
                  -> ([SSA.BasicBlock], MethodKnot)
                  -> (BlockNumber, Vector DT.Instruction)
                  -> KnotMonad f ([SSA.BasicBlock], MethodKnot)
translateBlock labeling tiedMknot (bs, mknot) (bnum, insts) = do
  bid <- freshId
  let blockPhis = M.findWithDefault [] bnum $ labelingBlockPhis labeling
      insts' = V.toList insts
      -- The last instruction has no successor
      nexts = drop 1 (map Just insts') ++ [Nothing]
  (phis, mknot') <- foldM (makePhi labeling tiedMknot) ([], mknot) blockPhis
  (insns, mknot'') <- foldM (translateInstruction labeling tiedMknot) ([], mknot') (zip insts' nexts)
  let b = SSA.BasicBlock { SSA.basicBlockId = bid
                         , SSA.basicBlockInstructions = V.fromList $ phis ++ reverse insns
                         , SSA.basicBlockPhiCount = length phis
                         }
  return (b : bs, mknot'' { mknotBlocks = M.insert bnum b (mknotBlocks mknot'') })

srcLabelForReg :: (Failure DecodeError f, FromRegister r)
                  => Labeling
                  -> r
                  -> KnotMonad f Label
srcLabelForReg = undefined

dstLabelForReg :: (Failure DecodeError f, FromRegister r)
                  => Labeling
                  -> r
                  -> KnotMonad f Label
dstLabelForReg = undefined

getFinalValue :: MethodKnot -> Label -> SSA.Value
getFinalValue mknot lbl =
  fromMaybe (error ("No value for label: " ++ show lbl)) $ M.lookup lbl (mknotValues mknot)

translateInstruction :: (Failure DecodeError f)
                        => Labeling
                        -> MethodKnot
                        -> ([SSA.Instruction], MethodKnot)
                        -> (DT.Instruction, Maybe DT.Instruction)
                        -> KnotMonad f ([SSA.Instruction], MethodKnot)
translateInstruction labeling tiedMknot acc@(insns, mknot) (inst, next) =
  case inst of
    -- These instructions do not show up in SSA form
    DT.Nop -> return acc
    DT.Move _ _ _ -> return acc
    DT.PackedSwitchData _ _ -> return acc
    DT.SparseSwitchData _ _ -> return acc
    DT.ArrayData _ _ _ -> return acc
    -- The rest of the instructions have some representation

    -- The only standalone Move1 is for moving exceptions off of the
    -- stack and into scope.  The rest will be associated with the
    -- instruction before them, and can be ignored.
    DT.Move1 MException dst -> do
      eid <- freshId
      lbl <- dstLabelForReg labeling dst
      let e = SSA.MoveException { instructionId = eid
                                }
      return (e : insns, addInstMap mknot lbl e)
    DT.Move1 _ _ -> return acc
    DT.ReturnVoid -> do
      rid <- freshId
      let r = SSA.Return { instructionId = rid
                         , instructionType = SSA.VoidType
                         , returnValue = Nothing
                         }
      return (r : insns, mknot)
    DT.Return _ src -> do
      rid <- freshId
      lbl <- srcLabelForReg labeling src
      let r = SSA.Return { instructionId = rid
                         , instructionType = SSA.VoidType
                         , returnValue = Just $ getFinalValue tiedMknot lbl
                         }
      return (r : insns, mknot)
    DT.MonitorEnter src -> do
      mid <- freshId
      lbl <- srcLabelForReg labeling src
      let m = SSA.MonitorEnter { instructionId = mid
                               , instructionType = SSA.VoidType
                               , monitorReference = getFinalValue tiedMknot lbl
                               }
      return (m : insns, mknot)
    DT.MonitorExit src -> do
      mid <- freshId
      lbl <- srcLabelForReg labeling src
      let m = SSA.MonitorExit { instructionId = mid
                              , instructionType = SSA.VoidType
                              , monitorReference = getFinalValue tiedMknot lbl
                              }
      return (m : insns, mknot)
    DT.CheckCast src tid -> do
      cid <- freshId
      lbl <- srcLabelForReg labeling src
      t <- getTranslatedType tid
      let c = SSA.CheckCast { instructionId = cid
                            , instructionType = SSA.VoidType
                            , castReference = getFinalValue tiedMknot lbl
                            , castType = t
                            }
      return (c : insns, mknot)
    DT.InstanceOf dst src tid -> do
      iid <- freshId
      srcLbl <- srcLabelForReg labeling src
      dstLbl <- dstLabelForReg labeling dst
      t <- getTranslatedType tid
      let i = SSA.InstanceOf { instructionId = iid
                             , instructionType = t
                             , instanceOfReference = getFinalValue tiedMknot srcLbl
                             }
      return (i : insns, addInstMap mknot dstLbl i)
    DT.ArrayLength dst src -> do
      aid <- freshId
      srcLbl <- srcLabelForReg labeling src
      dstLbl <- dstLabelForReg labeling dst
      let a = SSA.ArrayLength { instructionId = aid
                              , instructionType = IntType
                              , arrayReference = getFinalValue tiedMknot srcLbl
                              }
      return (a : insns, addInstMap mknot dstLbl a)
    DT.NewInstance dst tid -> do
      nid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      t <- getTranslatedType tid
      let n = SSA.NewInstance { instructionId = nid
                              , instructionType = t
                              }
      return (n : insns, addInstMap mknot dstLbl n)
    DT.NewArray dst src tid -> do
      nid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      srcLbl <- srcLabelForReg labeling src
      t <- getTranslatedType tid
      -- FIXME: Can we find array contents here?  fill-array isn't necessarily
      -- adjacent to the array allocation.  If it is, fine.  If it isn't, we still need
      -- the fill-array instruction...
      let n = SSA.NewArray { instructionId = nid
                           , instructionType = ArrayType t
                           , newArrayType = t
                           , newArrayLength = getFinalValue tiedMknot srcLbl
                           , newArrayContents = Nothing
                           }
      return (n : insns, addInstMap mknot dstLbl n)
    DT.Throw src -> do
      tid <- freshId
      srcLbl <- srcLabelForReg labeling src
      let t = SSA.Throw { instructionId = tid
                        , instructionType = VoidType
                        , throwReference = getFinalValue tiedMknot srcLbl
                        }
      return (t : insns, mknot)
    -- DT.Goto dstBlock -> do
    --   gid <- freshId
      
--    DT.LoadConst dst -> undefined -- This one is special and introduces constants.

addInstMap :: MethodKnot -> Label -> SSA.Instruction -> MethodKnot
addInstMap mknot lbl i = mknot { mknotValues = M.insert lbl (SSA.InstructionV i) (mknotValues mknot) }

-- | Make a phi node based on the labels we computed earlier.
makePhi :: (Failure DecodeError f)
           => Labeling
           -> MethodKnot
           -> ([SSA.Instruction], MethodKnot)
           -> Label
           -> KnotMonad f ([SSA.Instruction], MethodKnot)
makePhi labeling tiedMknot (insns, mknot) lbl@(PhiLabel _ _ _) = do
  phiId <- freshId
  --  Map Label (Set Label)
  let ivs = labelingPhiIncomingValues labeling lbl
      p = SSA.Phi { SSA.instructionId = phiId
                  , SSA.instructionType = undefined
                  , SSA.phiValues = map labelToIncoming ivs
                  }
  return (p : insns, mknot { mknotValues = M.insert lbl (InstructionV p) (mknotValues mknot) })
  where
    labelToIncoming (incBlock, incLbl) =
      (fromMaybe (error ("No block for incoming block id: " ++ show incBlock)) $ M.lookup incBlock (mknotBlocks tiedMknot),
       fromMaybe (error ("No value for incoming value: " ++ show incLbl)) $ M.lookup incLbl (mknotValues tiedMknot))
makePhi _ _ _ lbl = failure $ NonPhiLabelInBlockHeader $ show lbl


makeParameter :: (Failure DecodeError f)
                 => Map Int Parameter
                 -> (Int, (Maybe BS.ByteString, DT.TypeId))
                 -> KnotMonad f (Map Int Parameter)
makeParameter m (ix, (name, tid)) = do
  pid <- freshId
  t <- getTranslatedType tid
  let p = SSA.Parameter { SSA.parameterId = pid
                        , SSA.parameterType = t
                        , SSA.parameterName = fmap BS.unpack name
                        , SSA.parameterIndex = ix
                        }
  return $ M.insert ix p m


getRawField' :: (Failure DecodeError f) => DT.FieldId -> KnotMonad f DT.Field
getRawField' fid = do
  df <- gets knotDexFile
  lift $ getField df fid


getTranslatedType :: (Failure DecodeError f) => DT.TypeId -> KnotMonad f SSA.Type
getTranslatedType tid = do
  ts <- asks knotTypes
  let t = M.lookup tid ts
  maybe (error ("No SSA type for " ++ show tid)) return t

translateField :: (Failure DecodeError f) => DT.EncodedField -> KnotMonad f SSA.Field
translateField ef = do
  f <- getRawField' (DT.fieldId ef)
  fname <- getStr' (DT.fieldNameId f)
  ftype <- getTranslatedType (DT.fieldTypeId f)
  klass <- getTranslatedClass (DT.fieldClassId f)
  return SSA.Field { SSA.fieldId = fromIntegral $ DT.fieldId ef
                   , SSA.fieldAccessFlags = DT.fieldAccessFlags ef
                   , SSA.fieldName = fname
                   , SSA.fieldType = ftype
                   , SSA.fieldClass = klass
                   }

freshId :: (Failure DecodeError f) => KnotMonad f Int
freshId = do
  s <- get
  put s { knotIdSrc = knotIdSrc s + 1 }
  return $ knotIdSrc s

labelMethod :: (Failure DecodeError f) => DT.DexFile -> DT.EncodedMethod -> f Labeling
labelMethod _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
labelMethod dx em@(DT.EncodedMethod _ _ (Just codeItem)) = do
  insts <- DT.decodeInstructions (codeInsns codeItem)
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



{- Note [Translation]

Before building up the SSA-based IR, we label every Value with its
local SSA number using the algorithm from

  http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf

This is fairly different from the Cytron algorithm.  It works
backwards instead of forwards and does not require a dominance
frontier or a full CFG.  Once each value is identified this way,
making an SSA value for it should be simpler.

-}
