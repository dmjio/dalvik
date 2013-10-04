{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module defines an SSA-based IR for Dalvik.  The IR is a
-- cyclic data structure with as many references as possible resolved
-- for easy pattern matching.  Each value in the IR has identity
-- through a unique identifier (identifiers are only unique among
-- objects that can actually be compared for equality).
--
-- The organization of the data type parallels that of the low-level
-- Dalvik IR defined in Dalvik.Types and Dalvik.Instruction.  A
-- 'DexFile' is a collection of 'Class'es.  Each 'Class' has some
-- basic metadata (parent class, interfaces, flags, etc).  They also
-- contain their 'Method's and 'Field's.  'Field's are tagged with
-- their flags, while 'Method' flags are embedded within the 'Method'.
-- This is a minor inconsistency.  Note that class references (for
-- interfaces and parent classes) are through 'Type's and not direct
-- references.  This is because not all class definitions are
-- available in any given Dex file (notably, most core language and
-- Android platform classes are missing).  Direct references are
-- provided where possible.
--
-- 'Method's record basic metadata, along with a list of 'Parameter's
-- and possibly a method body.  Method bodies are present provided the
-- method is not abstract.  In the body of a 'Method', instructions
-- reference 'Value's.  A 'Value' can be 1) another 'Instruction', 2)
-- a 'Constant', or 3) a 'Parameter'.  The wrapper type 'Value' allows
-- us to refer to these constructs abstractly.  A 'Value' can be
-- pattern matched on to recover its real form.  Additionally, there
-- is a safe casting facility provided by the 'FromValue' class.
-- These safe casts are most useful with the Maybe monad and in
-- pattern guards.  For example, if you only care about 'Parameter's being
-- stored into object fields, you might do something like:
--
-- > paramStoredInField :: Instruction -> Maybe (Parameter, Field)
-- > paramStoredInField i = do
-- >   InstancePut { instanceOpReference = r
-- >               , instanceOpField = f
-- >               } <- fromValue i
-- >   p@Parameter {} <- fromValue r
-- >   return (p, f)
--
-- Obviously, this can also be accomplished with regular pattern
-- matching, but this style can be more convenient and restrict
-- nesting to more manageable levels.
--
-- The entry point to this module is 'toSSA', which converts Dalvik
-- into SSA form.
module Dalvik.SSA (
  toSSA,
  module Dalvik.SSA.Types,
  module Dalvik.SSA.Util
  ) where

import Control.Failure
import Control.Monad ( foldM, liftM )
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Int ( Int64 )
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import qualified Dalvik.AccessFlags as DT
import qualified Dalvik.Instruction as DT
import qualified Dalvik.Types as DT
import Dalvik.SSA.Types
import Dalvik.SSA.Types as SSA
import Dalvik.SSA.Util
import Dalvik.SSA.Internal.BasicBlocks as BB
import Dalvik.SSA.Internal.Labeling
import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Internal.RegisterAssignment
import Dalvik.SSA.Internal.Pretty ()

-- | Convert a 'Dalvik.Types.DexFile' into SSA form.  The result is a
-- different DexFile with as many references as possible resolved and
-- instructions in SSA form.  Some simple dead code elimination is
-- performed (unreachable exception handlers are removed).
--
-- The parameterized return type allows callers to choose how they
-- want to handle translation errors.  This function can be called purely
-- with the errors being returned via 'Either' or 'Maybe':
--
-- > maybe (error "Could not translate dex file") analyzeProgram dexfile
--
-- > either print analyzeProgram dexFile
--
-- Callers can also call 'toSSA' in the 'IO' monad and accept errors as
-- 'Exception's.
--
-- > main = do
-- >   dexfile = ...
-- >   ssafile <- toSSA dexfile
toSSA :: (MonadFix f, Failure DT.DecodeError f) => DT.DexFile -> f DexFile
toSSA df = do
  dexIdentifierStr <- case DT.dexThisId df of
    (-1) -> return "<none>"
    tid -> DT.getStr df tid
  tiedKnot <- mfix (tieKnot df)
  return DexFile { dexIdentifier = dexIdentifierStr
                 , dexClasses = M.elems (knotClasses tiedKnot)
                 , dexTypes = M.elems (knotTypes tiedKnot)
                 , dexConstants = knotConstants tiedKnot
                 }

tieKnot :: (MonadFix f, Failure DT.DecodeError f) => DT.DexFile -> Knot -> f Knot
tieKnot df tiedKnot = do
  knot' <- foldM (translateType df) emptyKnot $ M.toList (DT.dexTypeNames df)
  (k, s, _) <- runRWST startTranslation (tiedKnot, knot') (initialKnotState df)
  return k { knotConstants = concat [ M.elems $ knotClassConstantCache s
                                    , M.elems $ knotIntCache s
                                    , M.elems $ knotStringCache s
                                    ]
           }
  where
    startTranslation = do
      knot <- asks initialEnv
      knot' <- foldM translateFieldRef knot $ M.toList (DT.dexFields df)
      knot'' <- foldM translateMethodRef knot' $ M.toList (DT.dexMethods df)
      local (const (tiedKnot, knot'')) (translateClasses knot'')
    translateClasses knot = foldM translateClass knot $ M.toList (DT.dexClasses df)

-- | When querying the environment, grab the tied knot.  Note: be
-- careful about forcing anything you get out of the tied knot.
tiedEnv :: (Knot, Knot) -> Knot
tiedEnv = fst

-- | Access the un-tied environment.  For most of the translation
-- process, this value contains all translated types, method
-- references, and field references.  These can be accessed freely.
initialEnv :: (Knot, Knot) -> Knot
initialEnv = snd

type KnotMonad f = RWST (Knot, Knot) () KnotState f

-- | Before we start tying the knot, types, fields, and methodRefs are
-- all completely defined.
data Knot = Knot { knotClasses :: Map DT.TypeId Class
                 , knotMethodDefs :: Map DT.MethodId Method
                 , knotMethodRefs :: Map DT.MethodId MethodRef
                 , knotFields :: Map DT.FieldId Field
                 , knotTypes :: Map DT.TypeId Type
                 , knotConstants :: [Constant]
                 }

getMethodRef :: (Failure DT.DecodeError f) => DT.MethodId -> KnotMonad f MethodRef
getMethodRef mid = do
  mrefs <- asks (knotMethodRefs . initialEnv)
  maybe (failure (DT.NoMethodAtIndex mid)) return $ M.lookup mid mrefs

emptyKnot :: Knot
emptyKnot  = Knot { knotClasses = M.empty
                  , knotMethodDefs = M.empty
                  , knotMethodRefs = M.empty
                  , knotFields = M.empty
                  , knotTypes = M.empty
                  , knotConstants = []
                  }

data KnotState =
  KnotState { knotIdSrc :: Int
            , knotDexFile :: DT.DexFile
            , knotStringCache :: Map DT.StringId Constant
            , knotIntCache :: Map Int64 Constant
            , knotClassConstantCache :: Map DT.TypeId Constant
            }

initialKnotState :: DT.DexFile -> KnotState
initialKnotState df =
  KnotState { knotIdSrc = 0
            , knotDexFile = df
            , knotStringCache = M.empty
            , knotIntCache = M.empty
            , knotClassConstantCache = M.empty
            }

translateType :: (Failure DT.DecodeError f)
                 => DT.DexFile
                 -> Knot
                 -> (DT.TypeId, DT.StringId)
                 -> f Knot
translateType df m (tid, _) = do
  tname <- DT.getTypeName df tid
  ty <- parseTypeName tname
  return m { knotTypes = M.insert tid ty (knotTypes m) }

getStr' :: (Failure DT.DecodeError f) => DT.StringId -> KnotMonad f BS.ByteString
getStr' sid
  | sid == -1 = error "Missing string bt"
  | otherwise = do
    df <- gets knotDexFile
    lift $ DT.getStr df sid

lookupClass :: (Failure DT.DecodeError f)
               => DT.TypeId
               -> KnotMonad f (Maybe Class)
lookupClass tid = do
  klasses <- asks (knotClasses . tiedEnv)
  return $ M.lookup tid klasses

translateClass :: (MonadFix f, Failure DT.DecodeError f)
                  => Knot
                  -> (DT.TypeId, DT.Class)
                  -> KnotMonad f Knot
translateClass k (tid, klass) = do
  cid <- freshId
  sname <- case DT.classSourceNameId klass of
    (-1) -> return "<stdin>"
    snid -> getStr' snid
  parent <- case DT.classSuperId klass of
    (-1) -> return Nothing
    sid -> liftM Just $ getTranslatedType sid
  t <- getTranslatedType (DT.classId klass)
  parentRef <- lookupClass (DT.classSuperId klass)
  staticFields <- mapM (translateField k) (DT.classStaticFields klass)
  instanceFields <- mapM (translateField k) (DT.classInstanceFields klass)
  directMethods <- mapM translateMethod (DT.classDirectMethods klass)
  virtualMethods <- mapM translateMethod (DT.classVirtualMethods klass)
  itypes <- mapM getTranslatedType (DT.classInterfaces klass)
  let c = Class { classId = cid
                , classType = t
                , className = BS.pack $ show t
                , classSourceName = sname
                , classAccessFlags = DT.classAccessFlags klass
                , classParent = parent
                , classParentReference = parentRef
                , classInterfaces = itypes
                , classStaticFields = staticFields
                , classInstanceFields = instanceFields
                , classDirectMethods = directMethods
                , classVirtualMethods = virtualMethods
                }

  return k { knotClasses = M.insert tid c (knotClasses k) }

getRawMethod' :: (Failure DT.DecodeError f) => DT.MethodId -> KnotMonad f DT.Method
getRawMethod' mid = do
  df <- gets knotDexFile
  lift $ DT.getMethod df mid

getRawProto' :: (Failure DT.DecodeError f) => DT.ProtoId -> KnotMonad f DT.Proto
getRawProto' pid = do
  df <- gets knotDexFile
  lift $ DT.getProto df pid

translateMethod :: (MonadFix f, Failure DT.DecodeError f) => DT.EncodedMethod -> KnotMonad f Method
translateMethod em = do
  m <- getRawMethod' (DT.methId em)
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)

  df <- gets knotDexFile

  paramList <- lift $ getParamList df em
  paramMap <- foldM makeParameter M.empty (zip [0..] paramList)

  (body, _) <- mfix $ \tiedKnot ->
    translateMethodBody df paramMap (snd tiedKnot) em

  return Method { methodId = fromIntegral (DT.methId em)
                    , methodName = mname
                    , methodReturnType = rt
                    , methodAccessFlags = DT.methAccessFlags em
                    , methodParameters = M.elems paramMap
                    , methodBody = body
                    }

makeParameter :: (Failure DT.DecodeError f)
                 => Map Int Parameter
                 -> (Int, (Maybe BS.ByteString, DT.TypeId))
                 -> KnotMonad f (Map Int Parameter)
makeParameter m (ix, (name, tid)) = do
  pid <- freshId
  t <- getTranslatedType tid
  let p = Parameter { parameterId = pid
                        , parameterType = t
                        , parameterName = fromMaybe (generateNameForParameter ix) name
                        , parameterIndex = ix
                        }
  return $ M.insert ix p m

translateMethodBody :: (MonadFix f, Failure DT.DecodeError f)
                       => DT.DexFile
                       -> Map Int Parameter
                       -> MethodKnot
                       -> DT.EncodedMethod
                       -> KnotMonad f (Maybe [BasicBlock], MethodKnot)
translateMethodBody _ _ _ DT.EncodedMethod { DT.methCode = Nothing } = return (Nothing, emptyMethodKnot)
translateMethodBody df paramMap tiedMknot em = do
  labeling <- lift $ labelMethod df em
  let parameterLabels = labelingParameters labeling
      bbs = labelingBasicBlocks labeling
      blockList = basicBlocksAsList bbs
  mknot0 <- foldM addParameterLabel emptyMethodKnot parameterLabels
  (bs, resultKnot) <- foldM (translateBlock labeling tiedMknot) ([], mknot0) blockList
  return (Just (reverse bs), resultKnot)
  where
    addParameterLabel mknot l@(ArgumentLabel _ ix) =
      case M.lookup ix paramMap of
        Nothing -> failure $ DT.NoParameterAtIndex (DT.methId em) ix
        Just param ->
          return mknot { mknotValues = M.insert l (ParameterV param) (mknotValues mknot) }
    addParameterLabel _ l = failure $ DT.NonArgumentLabelInParameterList (DT.methId em) (show l)

data MethodKnot = MethodKnot { mknotValues :: Map Label Value
                             , mknotBlocks :: Map BlockNumber BasicBlock
                             }

emptyMethodKnot :: MethodKnot
emptyMethodKnot = MethodKnot M.empty M.empty

-- | To translate a BasicBlock, we first construct any (non-trivial)
-- phi nodes for the block.  Then translate each instruction.
--
-- Note that the phi nodes (if any) are all at the beginning of the
-- block in an arbitrary order.  Any analysis should process all of
-- the phi nodes for a single block at once.
translateBlock :: (Failure DT.DecodeError f)
                  => Labeling
                  -> MethodKnot
                  -> ([BasicBlock], MethodKnot)
                  -> (BlockNumber, Int, Vector DT.Instruction)
                  -> KnotMonad f ([BasicBlock], MethodKnot)
translateBlock labeling tiedMknot (bs, mknot) (bnum, indexStart, insts) = do
  bid <- freshId
  let blockPhis = M.findWithDefault [] bnum $ labelingBlockPhis labeling
      insts' = V.toList insts
  (phis, mknot') <- foldM (makePhi labeling tiedMknot) ([], mknot) blockPhis
  (insns, mknot'') <- foldM (translateInstruction labeling tiedMknot bnum) ([], mknot') (zip [indexStart..] insts')
  let b = BasicBlock { basicBlockId = bid
                     , basicBlockNumber = bnum
                     , basicBlockInstructions = V.fromList $ phis ++ reverse insns
                     , basicBlockPhiCount = length phis
                     , SSA.basicBlockSuccessors = map (getFinalBlock tiedMknot) $ BB.basicBlockSuccessors bbs bnum
                     , SSA.basicBlockPredecessors = map (getFinalBlock tiedMknot) $ BB.basicBlockPredecessors bbs bnum
                     }
  return (b : bs, mknot'' { mknotBlocks = M.insert bnum b (mknotBlocks mknot'') })
  where
    bbs = labelingBasicBlocks labeling

-- FIXME: We could insert unconditional branches at the end of any
-- basic blocks that fallthrough without an explicit transfer
-- instruction...  That might not be useful, though, since there are
-- already implicit terminators in the presence of exceptions.

srcLabelForReg :: (Failure DT.DecodeError f, FromRegister r)
                  => Labeling
                  -> Int
                  -> r
                  -> KnotMonad f Label
srcLabelForReg l ix r =
  maybe err return $ do
    regMap <- M.lookup ix (labelingReadRegs l)
    M.lookup (fromRegister r) regMap
  where
    err = failure $ DT.NoLabelForExpectedRegister "source" (fromRegister r) ix

dstLabelForReg :: (Failure DT.DecodeError f, FromRegister r)
                  => Labeling
                  -> Int
                  -> r
                  -> KnotMonad f Label
dstLabelForReg l ix r =
  maybe err return $ M.lookup ix (labelingWriteRegs l)
  where
    err = failure $ DT.NoLabelForExpectedRegister "destination" (fromRegister r) ix

getFinalValue :: MethodKnot -> Label -> Value
getFinalValue mknot lbl =
  fromMaybe (error ("No value for label: " ++ show lbl)) $ M.lookup lbl (mknotValues mknot)

getFinalBlock :: MethodKnot -> BlockNumber -> BasicBlock
getFinalBlock mknot bnum =
  fromMaybe (error ("No basic block: " ++ show bnum)) $ M.lookup bnum (mknotBlocks mknot)

translateInstruction :: forall f . (Failure DT.DecodeError f)
                        => Labeling
                        -> MethodKnot
                        -> BlockNumber
                        -> ([Instruction], MethodKnot)
                        -> (Int, DT.Instruction)
                        -> KnotMonad f ([Instruction], MethodKnot)
translateInstruction labeling tiedMknot bnum acc@(insns, mknot) (instIndex, inst) =
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
    DT.Move1 DT.MException dst -> do
      eid <- freshId
      lbl <- dstLabel dst
      exceptionType <- typeOfHandledException labeling bnum
      let e = MoveException { instructionId = eid
                            , instructionType = exceptionType
                            }
      return (e : insns, addInstMapping mknot lbl e)
    DT.Move1 _ _ -> return acc
    DT.ReturnVoid -> do
      rid <- freshId
      let r = Return { instructionId = rid
                         , instructionType = VoidType
                         , returnValue = Nothing
                         }
      return (r : insns, mknot)
    DT.Return _ src -> do
      rid <- freshId
      lbl <- srcLabel src
      let r = Return { instructionId = rid
                         , instructionType = VoidType
                         , returnValue = Just $ getFinalValue tiedMknot lbl
                         }
      return (r : insns, mknot)
    DT.MonitorEnter src -> do
      mid <- freshId
      lbl <- srcLabel src
      let m = MonitorEnter { instructionId = mid
                               , instructionType = VoidType
                               , monitorReference = getFinalValue tiedMknot lbl
                               }
      return (m : insns, mknot)
    DT.MonitorExit src -> do
      mid <- freshId
      lbl <- srcLabel src
      let m = MonitorExit { instructionId = mid
                              , instructionType = VoidType
                              , monitorReference = getFinalValue tiedMknot lbl
                              }
      return (m : insns, mknot)
    DT.CheckCast src tid -> do
      cid <- freshId
      srcLbl <- srcLabel src
      dstLbl <- dstLabel src
      t <- getTranslatedType tid
      let c = CheckCast { instructionId = cid
                        , instructionType = t
                        , castReference = getFinalValue tiedMknot srcLbl
                        , castType = t
                        }
      return (c : insns, addInstMapping mknot dstLbl c)
    DT.InstanceOf dst src tid -> do
      iid <- freshId
      srcLbl <- srcLabel src
      dstLbl <- dstLabel dst
      t <- getTranslatedType tid
      let i = InstanceOf { instructionId = iid
                             , instructionType = t
                             , instanceOfReference = getFinalValue tiedMknot srcLbl
                             }
      return (i : insns, addInstMapping mknot dstLbl i)
    DT.ArrayLength dst src -> do
      aid <- freshId
      srcLbl <- srcLabel src
      dstLbl <- dstLabel dst
      let a = ArrayLength { instructionId = aid
                              , instructionType = IntType
                              , arrayReference = getFinalValue tiedMknot srcLbl
                              }
      return (a : insns, addInstMapping mknot dstLbl a)
    DT.NewInstance dst tid -> do
      nid <- freshId
      dstLbl <- dstLabel dst
      t <- getTranslatedType tid
      let n = NewInstance { instructionId = nid
                              , instructionType = t
                              }
      return (n : insns, addInstMapping mknot dstLbl n)
    DT.NewArray dst src tid -> do
      nid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src
      t <- getTranslatedType tid
      let n = NewArray { instructionId = nid
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
    -- dimensions of the new array.  The real construction of
    -- multi-dimensional arrays is delegated to
    -- @java.lang.reflect.Array.newInstance@.
    DT.FilledNewArray tid srcRegs -> do
      nid <- freshId
      t <- getTranslatedType tid
      lbls <- mapM srcLabel srcRegs
      c <- getConstantInt (length srcRegs)
      let n = NewArray { instructionId = nid
                           , instructionType = t
                           , newArrayLength = c
                           , newArrayContents = Just $ map (getFinalValue tiedMknot) lbls
                           }
      -- We have to check the next instruction to see if the result of
      -- this instruction is saved anywhere.  If it is, the
      -- instruction introduces a new SSA value (the new array).
      possibleDestination <- resultSavedAs labeling instIndex
      case possibleDestination of
        Nothing -> return (n : insns, mknot)
        Just dstLbl -> return (n : insns, addInstMapping mknot dstLbl n)
    DT.FilledNewArrayRange tid srcRegs -> do
      nid <- freshId
      t <- getTranslatedType tid
      lbls <- mapM srcLabel srcRegs
      c <- getConstantInt (length srcRegs)
      let n = NewArray { instructionId = nid
                           , instructionType = t
                           , newArrayLength = c
                           , newArrayContents = Just $ map (getFinalValue tiedMknot) lbls
                           }
      -- We have to check the next instruction to see if the result of
      -- this instruction is saved anywhere.  If it is, the
      -- instruction introduces a new SSA value (the new array).
      possibleDestination <- resultSavedAs labeling instIndex
      case possibleDestination of
        Nothing -> return (n : insns, mknot)
        Just dstLbl -> return (n : insns, addInstMapping mknot dstLbl n)
    DT.FillArrayData src off -> do
      aid <- freshId
      srcLbl <- srcLabel src
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
          let a = FillArray { instructionId = aid
                                , instructionType = VoidType
                                , fillArrayReference = getFinalValue tiedMknot srcLbl
                                , fillArrayContents = numbers
                                }
          in return (a : insns, mknot)
        _ -> failure $ DT.NoArrayDataForFillArray instIndex
    DT.Throw src -> do
      tid <- freshId
      srcLbl <- srcLabel src
      let t = Throw { instructionId = tid
                        , instructionType = VoidType
                        , throwReference = getFinalValue tiedMknot srcLbl
                        }
      return (t : insns, mknot)

    DT.Cmp op dst src1 src2 -> do
      cid <- freshId
      dstLbl <- dstLabel dst
      src1Lbl <- srcLabel src1
      src2Lbl <- srcLabel src2
      let c = Compare { instructionId = cid
                          , instructionType = IntType
                          , compareOperation = op
                          , compareOperand1 = getFinalValue tiedMknot src1Lbl
                          , compareOperand2 = getFinalValue tiedMknot src2Lbl
                          }
      return (c : insns, addInstMapping mknot dstLbl c)

    DT.ArrayOp op dstOrSrc arry ix -> do
      aid <- freshId
      arryLbl <- srcLabel arry
      ixLbl <- srcLabel ix
      case op of
        DT.Put _ -> do
          pvLbl <- srcLabel dstOrSrc
          let a = ArrayPut { instructionId = aid
                               , instructionType = VoidType
                               , arrayReference = getFinalValue tiedMknot arryLbl
                               , arrayIndex = getFinalValue tiedMknot ixLbl
                               , arrayPutValue = getFinalValue tiedMknot pvLbl
                               }
          return (a : insns, mknot)
        DT.Get _ -> do
          dstLbl <- dstLabel dstOrSrc
          let a = ArrayGet { instructionId = aid
                               , instructionType =
                                 case typeOfLabel mknot arryLbl of
                                   ArrayType t -> t
                                   _ -> UnknownType
                               , arrayReference = getFinalValue tiedMknot arryLbl
                               , arrayIndex = getFinalValue tiedMknot ixLbl
                               }
          return (a : insns, addInstMapping mknot dstLbl a)
    DT.InstanceFieldOp op dstOrSrc objLbl field -> do
      iid <- freshId
      f <- getTranslatedField field
      refLbl <- srcLabel objLbl
      case op of
        DT.Put _ -> do
          valLbl <- srcLabel dstOrSrc
          let i = InstancePut { instructionId = iid
                                  , instructionType = VoidType
                                  , instanceOpReference = getFinalValue tiedMknot refLbl
                                  , instanceOpField = f
                                  , instanceOpPutValue = getFinalValue tiedMknot valLbl
                                  }
          return (i : insns, mknot)
        DT.Get _ -> do
          dstLbl <- dstLabel dstOrSrc
          let i = InstanceGet { instructionId = iid
                                  , instructionType = fieldType f
                                  , instanceOpReference = getFinalValue tiedMknot refLbl
                                  , instanceOpField = f
                                  }
          return (i : insns, addInstMapping mknot dstLbl i)
    DT.StaticFieldOp op dstOrSrc fid -> do
      sid <- freshId
      f <- getTranslatedField fid
      case op of
        DT.Put _ -> do
          valLbl <- srcLabel dstOrSrc
          let s = StaticPut { instructionId = sid
                                , instructionType = VoidType
                                , staticOpField = f
                                , staticOpPutValue = getFinalValue tiedMknot valLbl
                                }
          return (s : insns, mknot)
        DT.Get _ -> do
          dstLbl <- dstLabel dstOrSrc
          let s = StaticGet { instructionId = sid
                                , instructionType = fieldType f
                                , staticOpField = f
                                }
          return (s : insns, addInstMapping mknot dstLbl s)
    DT.Unop op dst src -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src
      let o = UnaryOp { instructionId = oid
                          , instructionType = unaryOpType op
                          , unaryOperand = getFinalValue tiedMknot srcLbl
                          , unaryOperation = op
                          }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.IBinop op isWide dst src1 src2 -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      src1Lbl <- srcLabel src1
      src2Lbl <- srcLabel src2
      let o = BinaryOp { instructionId = oid
                                             -- FIXME: Can this be short or byte?
                           , instructionType = if isWide then LongType else IntType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.FBinop op isWide dst src1 src2 -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      src1Lbl <- srcLabel src1
      src2Lbl <- srcLabel src2
      let o = BinaryOp { instructionId = oid
                           , instructionType = if isWide then DoubleType else FloatType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.IBinopAssign op isWide dstAndSrc src -> do
      oid <- freshId
      dstLbl <- dstLabel dstAndSrc
      src1Lbl <- srcLabel dstAndSrc
      src2Lbl <- srcLabel src
      let o = BinaryOp { instructionId = oid
                           , instructionType = if isWide then LongType else IntType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.FBinopAssign op isWide dstAndSrc src -> do
      oid <- freshId
      dstLbl <- dstLabel dstAndSrc
      src1Lbl <- srcLabel dstAndSrc
      src2Lbl <- srcLabel src
      let o = BinaryOp { instructionId = oid
                           , instructionType = if isWide then DoubleType else FloatType
                           , binaryOperand1 = getFinalValue tiedMknot src1Lbl
                           , binaryOperand2 = getFinalValue tiedMknot src2Lbl
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.BinopLit16 op dst src1 lit -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src1
      c <- getConstantInt lit
      let o = BinaryOp { instructionId = oid
                           , instructionType = IntType
                           , binaryOperand1 = getFinalValue tiedMknot srcLbl
                           , binaryOperand2 = c
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.BinopLit8 op dst src1 lit -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src1
      c <- getConstantInt lit
      let o = BinaryOp { instructionId = oid
                           , instructionType = IntType
                           , binaryOperand1 = getFinalValue tiedMknot srcLbl
                           , binaryOperand2 = c
                           , binaryOperation = op
                           }
      return (o : insns, addInstMapping mknot dstLbl o)
    DT.LoadConst dst cnst -> do
      c <- getConstant cnst
      dstLbl <- dstLabel dst
      return (insns, addValueMapping mknot dstLbl c)
    DT.Goto _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = UnconditionalBranch { instructionId = bid
                                      , instructionType = VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.Goto16 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = UnconditionalBranch { instructionId = bid
                                      , instructionType = VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.Goto32 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = UnconditionalBranch { instructionId = bid
                                      , instructionType = VoidType
                                      , branchTarget = getFinalBlock tiedMknot targetBlock
                                      }
      return (b : insns, mknot)
    DT.PackedSwitch src _ -> do
      bid <- freshId
      srcLbl <- srcLabel src
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], caseEdges) = L.partition isFallthroughEdge targets
          toSwitchTarget (c, t) =
            let SwitchCase val = c
            in (val, getFinalBlock tiedMknot t)
          b = Switch { instructionId = bid
                         , instructionType = VoidType
                         , switchValue = getFinalValue tiedMknot srcLbl
                         , switchTargets = map toSwitchTarget caseEdges
                         , switchFallthrough = getFinalBlock tiedMknot ft
                         }
      return (b : insns, mknot)
    DT.SparseSwitch src _ -> do
      bid <- freshId
      srcLbl <- srcLabel src
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], caseEdges) = L.partition isFallthroughEdge targets
          toSwitchTarget (c, t) =
            let SwitchCase val = c
            in (val, getFinalBlock tiedMknot t)
          b = Switch { instructionId = bid
                         , instructionType = VoidType
                         , switchValue = getFinalValue tiedMknot srcLbl
                         , switchTargets = map toSwitchTarget caseEdges
                         , switchFallthrough = getFinalBlock tiedMknot ft
                         }
      return (b : insns, mknot)
    DT.If op src1 src2 _ -> do
      bid <- freshId
      src1Lbl <- srcLabel src1
      src2Lbl <- srcLabel src2
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], [(Conditional, ct)]) = L.partition isFallthroughEdge targets
          b = ConditionalBranch { instructionId = bid
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
      srcLbl <- srcLabel src
      zero <- getConstantInt (0 :: Int)
      let targets = basicBlockBranchTargets bbs bnum
          ([(Fallthrough, ft)], [(Conditional, ct)]) = L.partition isFallthroughEdge targets
          b = ConditionalBranch { instructionId = bid
                                    , instructionType = VoidType
                                    , branchOperand1 = getFinalValue tiedMknot srcLbl
                                    , branchOperand2 = zero
                                    , branchTestType = op
                                    , branchTarget = getFinalBlock tiedMknot ct
                                    , branchFallthrough = getFinalBlock tiedMknot ft
                                    }
      return (b : insns, mknot)
    DT.Invoke ikind _isVarArg mId argRegs -> do
      df <- gets knotDexFile
      argRegs' <- lift $ filterWidePairs df mId ikind argRegs
      srcLbls <- mapM srcLabel argRegs'
      case ikind of
        DT.Virtual -> translateVirtualInvoke MethodInvokeVirtual mId srcLbls
        DT.Super -> translateVirtualInvoke MethodInvokeSuper mId srcLbls
        DT.Interface -> translateVirtualInvoke MethodInvokeInterface mId srcLbls
        DT.Direct -> translateDirectInvoke MethodInvokeDirect mId srcLbls
        DT.Static -> translateDirectInvoke MethodInvokeStatic mId srcLbls
  where
    dstLabel :: (Failure DT.DecodeError f, FromRegister r) => r -> KnotMonad f Label
    dstLabel = dstLabelForReg labeling instIndex
    srcLabel :: (Failure DT.DecodeError f, FromRegister r) => r -> KnotMonad f Label
    srcLabel = srcLabelForReg labeling instIndex
    bbs = labelingBasicBlocks labeling
    translateVirtualInvoke ikind mid argLbls = do
      iid <- freshId
      mref <- getMethodRef mid
      let i = InvokeVirtual { instructionId = iid
                                , instructionType = methodRefReturnType mref
                                , invokeVirtualKind = ikind
                                , invokeVirtualMethod = mref
                                , invokeArguments = map (getFinalValue tiedMknot) argLbls
                                }
      possibleDestination <- resultSavedAs labeling instIndex
      case possibleDestination of
        Nothing -> return (i : insns, mknot)
        Just dstLbl -> return (i : insns, addInstMapping mknot dstLbl i)
    translateDirectInvoke ikind mid argLbls = do
      iid <- freshId
      mref <- getMethodRef mid
      mdef <- getTranslatedMethod mid
      let i = InvokeDirect { instructionId = iid
                               , instructionType = methodRefReturnType mref
                               , invokeDirectKind = ikind
                               , invokeDirectMethod = mref
                               , invokeDirectMethodDef = mdef
                               , invokeArguments = map (getFinalValue tiedMknot) argLbls
                               }
      possibleDestination <- resultSavedAs labeling instIndex
      case possibleDestination of
        Nothing -> return (i : insns, mknot)
        Just dstLbl -> return (i : insns, addInstMapping mknot dstLbl i)



-- | Look up the exception handled in the given block, if any.  If the
-- block is not listed in a handler descriptor, that means this is a
-- finally block.  All we know in that case is that we have some
-- Throwable.
typeOfHandledException :: (Failure DT.DecodeError f) => Labeling -> BlockNumber -> KnotMonad f Type
typeOfHandledException labeling bnum =
  case basicBlockHandlesException (labelingBasicBlocks labeling) bnum of
    Just exname -> parseTypeName exname
    Nothing -> parseTypeName "Ljava/lang/Throwable;"

isFallthroughEdge :: (JumpCondition, BlockNumber) -> Bool
isFallthroughEdge (Fallthrough, _) = True
isFallthroughEdge _ = False

getConstant :: (Failure DT.DecodeError f) => DT.ConstArg -> KnotMonad f Value
getConstant ca =
  case ca of
    DT.Const4 i -> getConstantInt i
    DT.Const16 i -> getConstantInt i
    DT.Const32 i -> getConstantInt i
    DT.ConstHigh16 i -> getConstantInt i
    DT.ConstWide16 i -> getConstantInt i
    DT.ConstWide32 i -> getConstantInt i
    DT.ConstWide i -> getConstantInt i
    DT.ConstWideHigh16 i -> getConstantInt i
    DT.ConstString sid -> getConstantString sid
    DT.ConstStringJumbo sid -> getConstantString sid
    DT.ConstClass tid -> do
      s <- get
      case M.lookup tid (knotClassConstantCache s) of
        Just v -> return (ConstantV v)
        Nothing -> do
          cid <- freshId
          t <- getTranslatedType tid
          let c = ConstantClass cid t
          put s { knotClassConstantCache = M.insert tid c (knotClassConstantCache s) }
          return (ConstantV c)

getConstantString :: (Failure DT.DecodeError f) => DT.StringId -> KnotMonad f Value
getConstantString sid = do
  s <- get
  case M.lookup sid (knotStringCache s) of
    Just v -> return (ConstantV v)
    Nothing -> do
      cid <- freshId
      str <- getStr' sid
      let c = ConstantString cid str
      put s { knotStringCache = M.insert sid c (knotStringCache s) }
      return (ConstantV c)

getConstantInt :: (Failure DT.DecodeError f, Integral n) => n -> KnotMonad f Value
getConstantInt (fromIntegral -> i) = do
  s <- get
  case M.lookup i (knotIntCache s) of
    Just v -> return (ConstantV v)
    Nothing -> do
      cid <- freshId
      let c = ConstantInt cid i
      put s { knotIntCache = M.insert i c (knotIntCache s) }
      return (ConstantV c)

-- | Determine the result type of a unary operation
unaryOpType :: DT.Unop -> Type
unaryOpType o =
  case o of
    DT.NegInt -> IntType
    DT.NotInt -> IntType
    DT.NegLong -> LongType
    DT.NotLong -> LongType
    DT.NegFloat -> FloatType
    DT.NegDouble -> DoubleType
    DT.Convert _ ctype ->
      case ctype of
        DT.Byte -> ByteType
        DT.Char -> CharType
        DT.Short -> ShortType
        DT.Int -> IntType
        DT.Long -> LongType
        DT.Float -> FloatType
        DT.Double -> DoubleType

-- | We pass in the index of the instruction that might be returning a
-- value, not the index of the next instruction.
resultSavedAs :: (Failure DT.DecodeError f) => Labeling -> Int -> KnotMonad f (Maybe Label)
resultSavedAs labeling ix
  | Just (DT.Move1 _ dst) <- labelingInstructions labeling V.!? (ix + 1) =
    liftM Just $ dstLabelForReg labeling (ix + 1) dst
  | otherwise = return Nothing

-- | look up the type of a labeled value.  Note that we MUST only look
-- at values that are already defined.  Looking in the "final" tied
-- version of the state will lead to a <<loop>>.
typeOfLabel :: MethodKnot
               -> Label
               -> Type
typeOfLabel mknot lbl =
  case M.lookup lbl (mknotValues mknot) of
    Nothing -> UnknownType
    Just v -> valueType v

addValueMapping :: MethodKnot -> Label -> Value -> MethodKnot
addValueMapping mknot lbl v = mknot { mknotValues = M.insert lbl v (mknotValues mknot) }

addInstMapping :: MethodKnot -> Label -> Instruction -> MethodKnot
addInstMapping mknot lbl i = addValueMapping mknot lbl (InstructionV i)

-- | Make a phi node based on the labels we computed earlier.
makePhi :: (Failure DT.DecodeError f)
           => Labeling
           -> MethodKnot
           -> ([Instruction], MethodKnot)
           -> Label
           -> KnotMonad f ([Instruction], MethodKnot)
makePhi labeling tiedMknot (insns, mknot) lbl@(PhiLabel _ _ _) = do
  phiId <- freshId
  let ivs = labelingPhiIncomingValues labeling lbl
      p = Phi { instructionId = phiId
              , instructionType = UnknownType
              , phiValues = map labelToIncoming ivs
              }
  return (p : insns, mknot { mknotValues = M.insert lbl (InstructionV p) (mknotValues mknot) })
  where
    labelToIncoming (incBlock, incLbl) =
      (fromMaybe (error ("No block for incoming block id: " ++ show incBlock)) $ M.lookup incBlock (mknotBlocks tiedMknot),
       fromMaybe (error ("No value for incoming value: " ++ show incLbl)) $ M.lookup incLbl (mknotValues tiedMknot))
makePhi _ _ _ lbl = failure $ DT.NonPhiLabelInBlockHeader $ show lbl



-- | We do not consult the tied knot for types since we can translate
-- them all up-front.
getTranslatedType :: (Failure DT.DecodeError f) => DT.TypeId -> KnotMonad f Type
getTranslatedType tid = do
  ts <- asks (knotTypes . initialEnv)
  case M.lookup tid ts of
    Nothing -> failure $ DT.NoTypeAtIndex tid
    Just t -> return t

getTranslatedField :: (Failure DT.DecodeError f) => DT.FieldId -> KnotMonad f Field
getTranslatedField fid = do
  fs <- asks (knotFields . initialEnv)
  maybe (failure (DT.NoFieldAtIndex fid)) return $ M.lookup fid fs

getTranslatedMethod :: (Failure DT.DecodeError f) => DT.MethodId -> KnotMonad f (Maybe Method)
getTranslatedMethod mid = do
  ms <- asks (knotMethodDefs . tiedEnv)
  return $ M.lookup mid ms

-- | Translate an entry from the DexFile fields map.  These do not
-- contain access flags, but have everything else.
translateFieldRef :: (Failure DT.DecodeError f)
                     => Knot
                     -> (DT.FieldId, DT.Field)
                     -> KnotMonad f Knot
translateFieldRef knot (fid, f) = do
  fname <- getStr' (DT.fieldNameId f)
  ftype <- getTranslatedType (DT.fieldTypeId f)
  klass <- getTranslatedType (DT.fieldClassId f)
  let fld = Field { fieldId = fromIntegral fid
                      , fieldName = fname
                      , fieldType = ftype
                      , fieldClass = klass
                      }
  return knot { knotFields = M.insert fid fld (knotFields knot) }

translateMethodRef :: (Failure DT.DecodeError f)
                      => Knot
                      -> (DT.MethodId, DT.Method)
                      -> KnotMonad f Knot
translateMethodRef knot (mid, m) = do
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)
  cid <- getTranslatedType (DT.methClassId m)
  ptypes <- mapM getTranslatedType (DT.protoParams proto)

  let mref = MethodRef { methodRefId = fromIntegral mid
                           , methodRefClass = cid
                           , methodRefName = mname
                           , methodRefReturnType = rt
                           , methodRefParameterTypes = ptypes
                           }
  return knot { knotMethodRefs = M.insert mid mref (knotMethodRefs knot) }

translateField :: (Failure DT.DecodeError f) => Knot -> DT.EncodedField -> KnotMonad f (DT.AccessFlags, Field)
translateField knot ef =
  case M.lookup (DT.fieldId ef) (knotFields knot) of
    Nothing -> failure $ DT.NoFieldAtIndex (DT.fieldId ef)
    Just fref -> return (DT.fieldAccessFlags ef, fref)

-- | Allocate a fresh globally unique (within a single Dex file)
-- identifier.
freshId :: (Failure DT.DecodeError f) => KnotMonad f Int
freshId = do
  s <- get
  put s { knotIdSrc = knotIdSrc s + 1 }
  return $ knotIdSrc s





{- Note [Translation]

Before building up the SSA-based IR, we label every Value with its
local SSA number using the algorithm from

  http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf

This is fairly different from the Cytron algorithm.  It works
backwards instead of forwards and does not require a dominance
frontier or a full CFG.  Once each value is identified this way,
making an SSA value for it should be simpler.

-}
