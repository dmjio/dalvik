{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dalvik.SSA where

import Control.Arrow ( first )
import Control.Failure
import Control.Monad ( foldM, forM, liftM )
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word (Word16)

import Dalvik.AccessFlags as DT
import Dalvik.Instruction as DT
import Dalvik.Types as DT
import Dalvik.SSA.BasicBlocks
import Dalvik.SSA.Labeling
import Dalvik.SSA.Types as SSA
import Dalvik.SSA.Internal.Names

toSSA :: (MonadFix f, Failure DecodeError f) => DT.DexFile -> f SSA.DexFile
toSSA df = do
  dexIdentifierBS <- getStr df (dexThisId df)
  Knot { knotClasses = cmap } <- mfix (tieKnot df)
  return SSA.DexFile { dexIdentifier = BS.unpack dexIdentifierBS
                     , SSA.dexClasses = M.elems cmap
                     }

tieKnot :: (MonadFix f, Failure DecodeError f) => DT.DexFile -> Knot -> f Knot
tieKnot df tiedKnot = do
  knot' <- foldM (translateType df) emptyKnot $ M.toList (DT.dexTypeNames df)
  liftM fst $ evalRWST startTranslation (tiedKnot, knot') (initialKnotState df)
  where
    startTranslation = do
      knot <- asks initialEnv
      knot' <- foldM translateFieldRef knot $ M.toList (DT.dexFields df)
      knot'' <- foldM translateMethodRef knot' $ M.toList (DT.dexMethods df)
      local (const (tiedKnot, knot'')) (translateClasses knot'')
    translateClasses knot = foldM translateClass knot $ M.toList (DT.dexClasses df)

tiedEnv :: (Knot, Knot) -> Knot
tiedEnv = fst

initialEnv :: (Knot, Knot) -> Knot
initialEnv = snd

type KnotMonad f = RWST (Knot, Knot) () KnotState f

-- | Before we start tying the knot, types, fields, and methodRefs are
-- all completely defined.
data Knot = Knot { knotClasses :: Map DT.TypeId SSA.Class
                 , knotMethodDefs :: Map DT.MethodId SSA.Method
                 , knotMethodRefs :: Map DT.MethodId SSA.MethodRef
                 , knotFields :: Map DT.FieldId SSA.Field
                 , knotTypes :: Map DT.TypeId SSA.Type
                 }

getMethodRef :: (Failure DecodeError f) => DT.MethodId -> KnotMonad f SSA.MethodRef
getMethodRef mid = do
  mrefs <- asks (knotMethodRefs . initialEnv)
  maybe (failure (NoMethodAtIndex mid)) return $ M.lookup mid mrefs

emptyKnot :: Knot
emptyKnot  = Knot { knotClasses = M.empty
                  , knotMethodDefs = M.empty
                  , knotMethodRefs = M.empty
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
                 => DT.DexFile
                 -> Knot
                 -> (DT.TypeId, DT.StringId)
                 -> f Knot
translateType df m (tid, _) = do
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
  klasses <- asks (knotClasses . tiedEnv)
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
  staticFields <- mapM (translateField k) (DT.classStaticFields klass)
  instanceFields <- mapM (translateField k) (DT.classInstanceFields klass)
  directMethods <- mapM translateMethod (DT.classDirectMethods klass)
  virtualMethods <- mapM translateMethod (DT.classVirtualMethods klass)
  itypes <- mapM getTranslatedType (DT.classInterfaces klass)
  let c = SSA.Class { SSA.classId = cid
                    , SSA.className = cname
                    , SSA.classParent = parent
                    , SSA.classInterfaces = itypes
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
  (bs, tiedMknot, _) <- foldM (translateBlock labeling labelMap) ([], emptyMethodKnot, 0) blockList
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
                  -> ([SSA.BasicBlock], MethodKnot, Int)
                  -> (BlockNumber, Vector DT.Instruction)
                  -> KnotMonad f ([SSA.BasicBlock], MethodKnot, Int)
translateBlock labeling tiedMknot (bs, mknot, indexCounter) (bnum, insts) = do
  bid <- freshId
  let blockPhis = M.findWithDefault [] bnum $ labelingBlockPhis labeling
      insts' = V.toList insts
      -- The last instruction has no successor
      nexts = drop 1 (map Just insts') ++ [Nothing]
  (phis, mknot') <- foldM (makePhi labeling tiedMknot) ([], mknot) blockPhis
  (insns, mknot'') <- foldM (translateInstruction labeling tiedMknot bnum) ([], mknot') (zip3 [indexCounter..] insts' nexts)
  let b = SSA.BasicBlock { SSA.basicBlockId = bid
                         , SSA.basicBlockInstructions = V.fromList $ phis ++ reverse insns
                         , SSA.basicBlockPhiCount = length phis
                         }
  return (b : bs, mknot'' { mknotBlocks = M.insert bnum b (mknotBlocks mknot'') }, indexCounter + length insts')

-- FIXME: We could insert unconditional branches at the end of any
-- basic blocks that fallthrough without an explicit transfer
-- instruction...  That might not be useful, though, since there are
-- already implicit terminators in the presence of exceptions.

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

getFinalBlock :: MethodKnot -> BlockNumber -> SSA.BasicBlock
getFinalBlock mknot bnum =
  fromMaybe (error ("No basic block: " ++ show bnum)) $ M.lookup bnum (mknotBlocks mknot)

translateInstruction :: (Failure DecodeError f)
                        => Labeling
                        -> MethodKnot
                        -> BlockNumber
                        -> ([SSA.Instruction], MethodKnot)
                        -> (Int, DT.Instruction, Maybe DT.Instruction)
                        -> KnotMonad f ([SSA.Instruction], MethodKnot)
translateInstruction labeling tiedMknot bnum acc@(insns, mknot) (instIndex, inst, nextInst) =
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
      case basicBlockHandlesException (labelingBasicBlocks labeling) bnum of
        Just exname -> do
          ty <- parseTypeName exname
          let e = SSA.MoveException { instructionId = eid
                                    , instructionType = ty
                                    }
          return (e : insns, addInstMapping mknot lbl e)
        Nothing -> failure $ MoveExceptionOutsideOfHandler (show inst)
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
      return (i : insns, addInstMapping mknot dstLbl i)
    DT.ArrayLength dst src -> do
      aid <- freshId
      srcLbl <- srcLabelForReg labeling src
      dstLbl <- dstLabelForReg labeling dst
      let a = SSA.ArrayLength { instructionId = aid
                              , instructionType = SSA.IntType
                              , arrayReference = getFinalValue tiedMknot srcLbl
                              }
      return (a : insns, addInstMapping mknot dstLbl a)
    DT.NewInstance dst tid -> do
      nid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      t <- getTranslatedType tid
      let n = SSA.NewInstance { instructionId = nid
                              , instructionType = t
                              }
      return (n : insns, addInstMapping mknot dstLbl n)
    DT.NewArray dst src tid -> do
      nid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      srcLbl <- srcLabelForReg labeling src
      t <- getTranslatedType tid
      let n = SSA.NewArray { instructionId = nid
                           , instructionType = t
                           , newArrayLength = getFinalValue tiedMknot srcLbl
                           , newArrayContents = Nothing
                           }
      return (n : insns, addInstMapping mknot dstLbl n)
    -- As far as I can tell, this instruction (and its /range variant)
    -- does roughly what it says: it takes several registers and
    -- returns a new array with the values held in those registers as
    -- its contents.  Note that array initializer syntax does *not*
    -- get the compiler to generate these (that case uses
    -- fill-array-data).  The compiler only seems to emit these when
    -- setting up a multi-dimensional array allocation.  But the
    -- result of this instruction is only used to store the
    -- *dimensions* of the new array.  The real construction of
    -- multi-dimensional arrays is delegated to
    -- @java.lang.reflect.Array.newInstance@.
    DT.FilledNewArray tid srcRegs -> do
      nid <- freshId
      t <- getTranslatedType tid
      lbls <- mapM (srcLabelForReg labeling) srcRegs
      c <- getConstantInt (length srcRegs)
      let n = SSA.NewArray { instructionId = nid
                           , instructionType = t
                           , newArrayLength = c
                           , newArrayContents = Just $ map (getFinalValue tiedMknot) lbls
                           }
      -- We have to check the next instruction to see if the result of
      -- this instruction is saved anywhere.  If it is, the
      -- instruction introduces a new SSA value (the new array).
      possibleDestination <- resultSavedAs labeling nextInst
      case possibleDestination of
        Nothing -> return (n : insns, mknot)
        Just dstLbl -> return (n : insns, addInstMapping mknot dstLbl n)
    DT.FilledNewArrayRange tid srcRegs -> do
      nid <- freshId
      t <- getTranslatedType tid
      lbls <- mapM (srcLabelForReg labeling) srcRegs
      c <- getConstantInt (length srcRegs)
      let n = SSA.NewArray { instructionId = nid
                           , instructionType = t
                           , newArrayLength = c
                           , newArrayContents = Just $ map (getFinalValue tiedMknot) lbls
                           }
      -- We have to check the next instruction to see if the result of
      -- this instruction is saved anywhere.  If it is, the
      -- instruction introduces a new SSA value (the new array).
      possibleDestination <- resultSavedAs labeling nextInst
      case possibleDestination of
        Nothing -> return (n : insns, mknot)
        Just dstLbl -> return (n : insns, addInstMapping mknot dstLbl n)
    DT.FillArrayData src off -> do
      aid <- freshId
      srcLbl <- srcLabelForReg labeling src
      -- The array data payload is stored as Word16 (ushort).  The
      -- payload instruction tells us how many *bytes* are required
      -- for each actual data item.  If it is 1 (for bytes), then we
      -- need to split code units.  If it is 2, we have the raw data.
      -- If it is 4 or 8, we need to combine adjacent units.
      --
      -- If the dex file is endian swapped, we apparently need to
      -- byte swap each ushort.  Not doing that yet...
      case instructionAtRawOffsetFrom bbs instIndex off of
        Just (DT.ArrayData _ _ numbers) ->
          let a = SSA.FillArray { instructionId = aid
                                , instructionType = SSA.VoidType
                                , fillArrayReference = getFinalValue tiedMknot srcLbl
                                , fillArrayContents = numbers
                                }
          in return (a : insns, mknot)
        _ -> failure $ NoArrayDataForFillArray instIndex
    DT.Throw src -> do
      tid <- freshId
      srcLbl <- srcLabelForReg labeling src
      let t = SSA.Throw { instructionId = tid
                        , instructionType = SSA.VoidType
                        , throwReference = getFinalValue tiedMknot srcLbl
                        }
      return (t : insns, mknot)

    DT.Cmp op dst src1 src2 -> do
      cid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      src1Lbl <- srcLabelForReg labeling src1
      src2Lbl <- srcLabelForReg labeling src2
      let c = SSA.Compare { instructionId = cid
                          , instructionType = SSA.IntType
                          , compareOperation = op
                          , compareOperand1 = getFinalValue tiedMknot src1Lbl
                          , compareOperand2 = getFinalValue tiedMknot src2Lbl
                          }
      return (c : insns, addInstMapping mknot dstLbl c)

    DT.ArrayOp op dstOrSrc arry ix -> do
      aid <- freshId
      arryLbl <- srcLabelForReg labeling arry
      ixLbl <- srcLabelForReg labeling ix
      case op of
        Put _ -> do
          pvLbl <- srcLabelForReg labeling dstOrSrc
          let a = SSA.ArrayPut { instructionId = aid
                               , instructionType = SSA.VoidType
                               , arrayReference = getFinalValue tiedMknot arryLbl
                               , arrayIndex = getFinalValue tiedMknot ixLbl
                               , arrayPutValue = getFinalValue tiedMknot pvLbl
                               }
          return (a : insns, mknot)
        Get _ -> do
          dstLbl <- dstLabelForReg labeling dstOrSrc
          let a = SSA.ArrayGet { instructionId = aid
                               , instructionType =
                                 case typeOfLabel mknot arryLbl of
                                   SSA.ArrayType t -> t
                                   _ -> UnknownType
                               , arrayReference = getFinalValue tiedMknot arryLbl
                               , arrayIndex = getFinalValue tiedMknot ixLbl
                               }
          return (a : insns, addInstMapping mknot dstLbl a)
    DT.InstanceFieldOp op dstOrSrc objLbl field -> do
      iid <- freshId
      f <- getTranslatedField field
      refLbl <- srcLabelForReg labeling objLbl
      case op of
        Put _ -> do
          valLbl <- srcLabelForReg labeling dstOrSrc
          let i = SSA.InstancePut { instructionId = iid
                                  , instructionType = SSA.VoidType
                                  , instanceOpReference = getFinalValue tiedMknot refLbl
                                  , instanceOpField = f
                                  , instanceOpPutValue = getFinalValue tiedMknot valLbl
                                  }
          return (i : insns, mknot)
        Get _ -> do
          dstLbl <- dstLabelForReg labeling dstOrSrc
          t <- getFieldType field
          let i = SSA.InstanceGet { instructionId = iid
                                  , instructionType = t
                                  , instanceOpReference = getFinalValue tiedMknot refLbl
                                  , instanceOpField = f
                                  }
          return (i : insns, addInstMapping mknot dstLbl i)
    DT.StaticFieldOp op dstOrSrc fid -> do
      sid <- freshId
      f <- getTranslatedField fid
      case op of
        Put _ -> do
          valLbl <- srcLabelForReg labeling dstOrSrc
          let s = SSA.StaticPut { instructionId = sid
                                , instructionType = VoidType
                                , staticOpField = f
                                , staticOpPutValue = getFinalValue tiedMknot valLbl
                                }
          return (s : insns, mknot)
        Get _ -> do
          t <- getFieldType fid
          dstLbl <- dstLabelForReg labeling dstOrSrc
          let s = SSA.StaticGet { instructionId = sid
                                , instructionType = t
                                , staticOpField = f
                                }
          return (s : insns, addInstMapping mknot dstLbl s)
    DT.Unop op dst src -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      srcLbl <- srcLabelForReg labeling src
      let o = SSA.UnaryOp { instructionId = oid
                          , instructionType = unaryOpType op
                          , unaryOperand = getFinalValue tiedMknot srcLbl
                          , unaryOperation = op
                          }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.IBinop op isWide dst src1 src2 -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      src1Lbl <- srcLabelForReg labeling src1
      src2Lbl <- srcLabelForReg labeling src2
      let o = SSA.BinaryOp { instructionId = oid
                                             -- FIXME: Can this be short or byte?
                           , instructionType = if isWide then SSA.LongType else SSA.IntType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.FBinop op isWide dst src1 src2 -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      src1Lbl <- srcLabelForReg labeling src1
      src2Lbl <- srcLabelForReg labeling src2
      let o = SSA.BinaryOp { instructionId = oid
                           , instructionType = if isWide then SSA.DoubleType else SSA.FloatType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.IBinopAssign op isWide dstAndSrc src -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dstAndSrc
      src1Lbl <- srcLabelForReg labeling dstAndSrc
      src2Lbl <- srcLabelForReg labeling src
      let o = SSA.BinaryOp { instructionId = oid
                           , instructionType = if isWide then SSA.LongType else SSA.IntType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.FBinopAssign op isWide dstAndSrc src -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dstAndSrc
      src1Lbl <- srcLabelForReg labeling dstAndSrc
      src2Lbl <- srcLabelForReg labeling src
      let o = SSA.BinaryOp { instructionId = oid
                           , instructionType = if isWide then SSA.DoubleType else SSA.FloatType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.BinopLit16 op dst src1 lit -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      srcLbl <- srcLabelForReg labeling src1
      c <- getConstantInt lit
      let o = SSA.BinaryOp { instructionId = oid
                           , instructionType = SSA.IntType
                           , binaryOperand1 = getFinalValue tiedMknot srcLbl
                           , binaryOperand2 = c
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.BinopLit8 op dst src1 lit -> do
      oid <- freshId
      dstLbl <- dstLabelForReg labeling dst
      srcLbl <- srcLabelForReg labeling src1
      c <- getConstantInt lit
      let o = SSA.BinaryOp { instructionId = oid
                           , instructionType = SSA.IntType
                           , binaryOperand1 = getFinalValue tiedMknot srcLbl
                           , binaryOperand2 = c
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.LoadConst dst cnst -> do
      c <- getConstant cnst
      dstLbl <- dstLabelForReg labeling dst
      return (insns, addValueMapping mknot dstLbl c)
    DT.Goto _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = SSA.UnconditionalBranch { instructionId = bid
                                      , instructionType = SSA.VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.Goto16 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = SSA.UnconditionalBranch { instructionId = bid
                                      , instructionType = SSA.VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.Goto32 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = SSA.UnconditionalBranch { instructionId = bid
                                      , instructionType = SSA.VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.PackedSwitch src _ -> do
      bid <- freshId
      srcLbl <- srcLabelForReg labeling src
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], caseEdges) = L.partition isFallthroughEdge targets
          toSwitchTarget (c, t) =
            let SwitchCase val = c
            in (val, getFinalBlock tiedMknot t)
          b = SSA.Switch { instructionId = bid
                         , instructionType = SSA.VoidType
                         , switchValue = getFinalValue tiedMknot srcLbl
                         , switchTargets = map toSwitchTarget caseEdges
                         , switchFallthrough = getFinalBlock tiedMknot ft
                         }
      return (b : insns, mknot)
    DT.SparseSwitch src _ -> do
      bid <- freshId
      srcLbl <- srcLabelForReg labeling src
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], caseEdges) = L.partition isFallthroughEdge targets
          toSwitchTarget (c, t) =
            let SwitchCase val = c
            in (val, getFinalBlock tiedMknot t)
          b = SSA.Switch { instructionId = bid
                         , instructionType = SSA.VoidType
                         , switchValue = getFinalValue tiedMknot srcLbl
                         , switchTargets = map toSwitchTarget caseEdges
                         , switchFallthrough = getFinalBlock tiedMknot ft
                         }
      return (b : insns, mknot)
    DT.If op src1 src2 _ -> do
      bid <- freshId
      src1Lbl <- srcLabelForReg labeling src1
      src2Lbl <- srcLabelForReg labeling src2
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], [(Conditional, ct)]) = L.partition isFallthroughEdge targets
          b = SSA.ConditionalBranch { instructionId = bid
                                    , instructionType = VoidType
                                    , branchOperand1 = getFinalValue tiedMknot src1Lbl
                                    , branchOperand2 = getFinalValue tiedMknot src2Lbl
                                    , branchTestType = op
                                    , branchTarget = getFinalBlock tiedMknot ct
                                    , branchFallthrough = getFinalBlock tiedMknot ft
                                    }
      return (b : insns, mknot)
    DT.IfZero op src _ -> do
      bid <- freshId
      srcLbl <- srcLabelForReg labeling src
      zero <- getConstantInt (0 :: Int)
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], [(Conditional, ct)]) = L.partition isFallthroughEdge targets
          b = SSA.ConditionalBranch { instructionId = bid
                                    , instructionType = VoidType
                                    , branchOperand1 = getFinalValue tiedMknot srcLbl
                                    , branchOperand2 = zero
                                    , branchTestType = op
                                    , branchTarget = getFinalBlock tiedMknot ct
                                    , branchFallthrough = getFinalBlock tiedMknot ft
                                    }
      return (b : insns, mknot)
    DT.Invoke ikind _isVarArg mId argRegs -> do
      srcLbls <- mapM (srcLabelForReg labeling) argRegs
      case ikind of
        DT.Virtual -> translateVirtualInvoke MethodInvokeVirtual mId srcLbls
        DT.Super -> translateVirtualInvoke MethodInvokeSuper mId srcLbls
        DT.Interface -> translateVirtualInvoke MethodInvokeInterface mId srcLbls
        DT.Direct -> translateDirectInvoke MethodInvokeDirect mId srcLbls
        DT.Static -> translateDirectInvoke MethodInvokeStatic mId srcLbls

  where
    bbs = labelingBasicBlocks labeling
    translateVirtualInvoke ikind mid argLbls = do
      iid <- freshId
      mref <- getMethodRef mid
      let i = SSA.InvokeVirtual { instructionId = iid
                                , instructionType = methodRefReturnType mref
                                , invokeVirtualKind = ikind
                                , invokeVirtualMethod = mref
                                , invokeArguments = map (getFinalValue tiedMknot) argLbls
                                }
      possibleDestination <- resultSavedAs labeling nextInst
      case possibleDestination of
        Nothing -> return (i : insns, mknot)
        Just dstLbl -> return (i : insns, addInstMapping mknot dstLbl i)
    translateDirectInvoke ikind mid argLbls = do
      iid <- freshId
      mref <- getMethodRef mid
      mdef <- getTranslatedMethod mid
      let i = SSA.InvokeDirect { instructionId = iid
                               , instructionType = methodRefReturnType mref
                               , invokeDirectKind = ikind
                               , invokeDirectMethod = mref
                               , invokeDirectMethodDef = mdef
                               , invokeArguments = map (getFinalValue tiedMknot) argLbls
                               }
      possibleDestination <- resultSavedAs labeling nextInst
      case possibleDestination of
        Nothing -> return (i : insns, mknot)
        Just dstLbl -> return (i : insns, addInstMapping mknot dstLbl i)


isFallthroughEdge :: (JumpCondition, BlockNumber) -> Bool
isFallthroughEdge (Fallthrough, _) = True
isFallthroughEdge _ = False

getConstant :: (Failure DecodeError f) => n -> KnotMonad f SSA.Value
getConstant = undefined

getConstantInt :: (Failure DecodeError f, Integral n) => n -> KnotMonad f SSA.Value
getConstantInt = undefined

-- | Determine the result type of a unary operation
unaryOpType :: DT.Unop -> SSA.Type
unaryOpType o =
  case o of
    DT.NegInt -> SSA.IntType
    DT.NotInt -> SSA.IntType
    DT.NegLong -> SSA.LongType
    DT.NotLong -> SSA.LongType
    DT.NegFloat -> SSA.FloatType
    DT.NegDouble -> SSA.DoubleType
    DT.Convert _ ctype ->
      case ctype of
        DT.Byte -> SSA.ByteType
        DT.Char -> SSA.CharType
        DT.Short -> SSA.ShortType
        DT.Int -> SSA.IntType
        DT.Long -> SSA.LongType
        DT.Float -> SSA.FloatType
        DT.Double -> SSA.DoubleType

resultSavedAs :: (Failure DecodeError f) => Labeling -> Maybe DT.Instruction -> KnotMonad f (Maybe Label)
resultSavedAs labeling (Just (DT.Move1 _ dst)) =
  liftM Just $ dstLabelForReg labeling dst
resultSavedAs _ _ = return Nothing

-- | look up the type of a labeled value.  Note that we MUST only look
-- at values that are already defined.  Looking in the "final" tied
-- version of the state will lead to a <<loop>>.
typeOfLabel :: MethodKnot
               -> Label
               -> SSA.Type
typeOfLabel mknot lbl =
  case M.lookup lbl (mknotValues mknot) of
    Nothing -> SSA.UnknownType
    Just v -> valueType v

addValueMapping :: MethodKnot -> Label -> SSA.Value -> MethodKnot
addValueMapping mknot lbl v = mknot { mknotValues = M.insert lbl v (mknotValues mknot) }

addInstMapping :: MethodKnot -> Label -> SSA.Instruction -> MethodKnot
addInstMapping mknot lbl i = addValueMapping mknot lbl (SSA.InstructionV i)

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

-- | We do not consult the tied knot for types since we can translate
-- them all up-front.
getTranslatedType :: (Failure DecodeError f) => DT.TypeId -> KnotMonad f SSA.Type
getTranslatedType tid = do
  ts <- asks (knotTypes . initialEnv)
  case M.lookup tid ts of
    Nothing -> failure $ NoTypeAtIndex tid
    Just t -> return t

getTranslatedField :: (Failure DecodeError f) => DT.FieldId -> KnotMonad f SSA.Field
getTranslatedField fid = do
  fs <- asks (knotFields . initialEnv)
  maybe (failure (NoFieldAtIndex fid)) return $ M.lookup fid fs

getTranslatedMethod :: (Failure DecodeError f) => DT.MethodId -> KnotMonad f (Maybe SSA.Method)
getTranslatedMethod mid = do
  ms <- asks (knotMethodDefs . tiedEnv)
  return $  M.lookup mid ms

-- | This does not touch the tied knot and is safe to call from
-- anywhere (since Types are translated first).
getFieldType :: (Failure DecodeError f) => DT.FieldId -> KnotMonad f SSA.Type
getFieldType fid = do
  rawFields <- gets (DT.dexFields . knotDexFile)
  let Just rawField = M.lookup fid rawFields
  getTranslatedType (DT.fieldTypeId rawField)

translateFieldRef :: (Failure DecodeError f)
                     => Knot
                     -> (DT.FieldId, DT.Field)
                     -> KnotMonad f Knot
translateFieldRef knot (fid, f) = do
  fname <- getStr' (DT.fieldNameId f)
  ftype <- getTranslatedType (DT.fieldTypeId f)
  klass <- getTranslatedType (DT.fieldClassId f)
  let fld = SSA.Field { SSA.fieldId = fromIntegral fid
                      , SSA.fieldName = fname
                      , SSA.fieldType = ftype
                      , SSA.fieldClass = klass
                      }
  return knot { knotFields = M.insert fid fld (knotFields knot) }

translateMethodRef :: (Failure DecodeError f)
                      => Knot
                      -> (DT.MethodId, DT.Method)
                      -> KnotMonad f Knot
translateMethodRef knot (mid, m) = do
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)
  cid <- getTranslatedType (DT.methClassId m)
  ptypes <- mapM getTranslatedType (DT.protoParams proto)

  let mref = SSA.MethodRef { SSA.methodRefId = fromIntegral mid
                           , SSA.methodRefClass = cid
                           , SSA.methodRefName = mname
                           , SSA.methodRefReturnType = rt
                           , SSA.methodRefParameterTypes = ptypes
                           }
  return knot { knotMethodRefs = M.insert mid mref (knotMethodRefs knot) }

translateField :: (Failure DecodeError f) => Knot -> DT.EncodedField -> KnotMonad f (DT.AccessFlags, SSA.Field)
translateField knot ef = do
  case M.lookup (DT.fieldId ef) (knotFields knot) of
    Nothing -> failure $ NoFieldAtIndex (DT.fieldId ef)
    Just fref -> return (DT.fieldAccessFlags ef, fref)

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
