{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  Stubs,
  St.stubs,
  module Dalvik.SSA.Types,
  module Dalvik.SSA.Util,
  -- * Serialization
  module Dalvik.SSA.Internal.Serialize
  ) where

import qualified Control.Monad.Catch as E
import Control.Monad ( foldM, liftM )
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Int ( Int64 )
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, maybeToList )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Text.PrettyPrint.GenericPretty as PP

import qualified Dalvik.AccessFlags as DT
import qualified Dalvik.Instruction as DT
import qualified Dalvik.Types as DT
import Dalvik.SSA.Types hiding ( _dexClassesByType, _classStaticFieldMap, _classInstanceFieldMap )
import Dalvik.SSA.Types as SSA
import Dalvik.SSA.Util
import Dalvik.SSA.Internal.BasicBlocks as BB
import Dalvik.SSA.Internal.Labeling
import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Internal.Pretty ()
import Dalvik.SSA.Internal.RegisterAssignment
import Dalvik.SSA.Internal.Serialize
import Dalvik.SSA.Internal.Stubs ( Stubs )
import qualified Dalvik.SSA.Internal.Stubs as St

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
-- >   ssafile <- toSSA [dexfile]
toSSA :: (MonadFix m, E.MonadThrow m)
         => Maybe Stubs -- ^ Custom stub methods
         -> Maybe DexFile -- ^ A base Dex file to build on top of
         -> [DT.DexFile] -- ^ Input dex files (to be merged)
         -> m DexFile
toSSA mstubs mbase dfs = do
  tiedKnot <- mfix (tieKnot mstubs mbase dfs)
  let declaredTypes = HM.elems (knotTypes tiedKnot)
      classes = HM.elems (knotClasses tiedKnot)
      insnTypes = [ instructionType i
                  | klass <- classes
                  , m <- classMethods klass
                  , body <- maybeToList $ methodBody m
                  , block <- body
                  , i <- basicBlockInstructions block
                  ]
      -- See Note [Synthesized Types]
      allTypes = S.fromList $ concat [ [UnknownType]
                                     , declaredTypes
                                     , insnTypes
                                     ]
  return DexFile { _dexClasses = V.fromList classes
                 , _dexTypes = V.fromList (S.toList allTypes)
                 , _dexConstants = V.fromList (knotConstants tiedKnot)
                 , SSA._dexClassesByType =
                   HM.foldr addTypeMap HM.empty (knotClasses tiedKnot)
                 , SSA._dexClassesByName =
                   HM.foldr addNameMap HM.empty (knotClasses tiedKnot)
                 , dexIdSrc = knotMaxId tiedKnot
                 }
  where
    addNameMap klass m = HM.insert (className klass) klass m
    addTypeMap klass m = HM.insert (classType klass) klass m

-- | We tie the knot by starting with an empty knot and processing a
-- dex file at a time.  Each dex file proceeds by class.
tieKnot :: (MonadFix m, E.MonadThrow m)
           => Maybe Stubs
           -> Maybe DexFile
           -> [DT.DexFile]
           -> Knot
           -> m Knot
tieKnot mstubs mbase dfs tiedKnot = do
  (k, s, _) <- runRWST go tiedKnot (initialKnotState mstubs mbase)
  return k { knotConstants = concat [ HM.elems $ knotClassConstantCache s
                                    , M.elems $ knotIntCache s
                                    , HM.elems $ knotStringCache s
                                    ]
           , knotMaxId = knotIdSrc s
           }
  where
    go = foldM translateDex (emptyKnot mbase) dfs

translateDex :: (MonadFix m, E.MonadThrow m) => Knot -> DT.DexFile -> KnotMonad m Knot
translateDex knot0 df = do
  -- Note that we have to set the DexFile being processed here (it
  -- starts off undefined) and that we also reset the two caches that
  -- are only valid for a given dex file.
  modify $ \s -> s { knotDexFile = df
                   , knotDexFields = M.empty
                   , knotDexMethods = M.empty
                   }
  knot1 <- foldM (translateType df) knot0 $ M.toList (DT.dexTypeNames df)
  -- After we have translated all of the types in this dex file, we
  -- save them in the state so we can safely refer to them (and
  -- inspect them) without touching the knot, which is more delicate.
  modify $ \s -> s { knotDexTypes = knotTypes knot1 }
  knot2 <- foldM translateFieldRef knot1 $ M.toList (DT.dexFields df)
  knot3 <- foldM translateMethodRef knot2 $ M.toList (DT.dexMethods df)
  foldM translateClass knot3 $ M.toList (DT.dexClasses df)

type KnotMonad m = RWST Knot () KnotState m

-- | Before we start tying the knot, types, fields, and methodRefs are
-- all completely defined.
data Knot = Knot { knotClasses :: !(HashMap BS.ByteString Class)
                 , knotMethodDefs :: !(HashMap (BS.ByteString, BS.ByteString, [Type]) Method)
                 , knotMethodRefs :: !(HashMap (BS.ByteString, BS.ByteString, [Type]) MethodRef)
                 , knotFields :: !(HashMap (BS.ByteString, BS.ByteString) Field)
                 , knotTypes :: !(HashMap BS.ByteString Type)
                 , knotConstants :: [Constant]
                 , knotMaxId :: !Int
                 }

getMethodRef :: (E.MonadThrow m) => DT.MethodId -> KnotMonad m MethodRef
getMethodRef mid = do
  mkeys <- gets knotDexMethods
  case M.lookup mid mkeys of
    Nothing -> E.throwM $ DT.NoMethodAtIndex mid
    Just stringKey -> do
      mrefs <- asks knotMethodRefs
      let errMsg = error ("No method for method id " ++ show mid)
      return $ fromMaybe errMsg $ HM.lookup stringKey mrefs

-- | Create an initial knot, possibly including information from a
-- base dex file.  We don't need to set the id or constants fields
-- because those are always just populated after the knot is tied.
emptyKnot :: Maybe DexFile -> Knot
emptyKnot mbase =
  Knot { knotClasses = maybe HM.empty collectClasses mbase
       , knotMethodDefs = maybe HM.empty collectMethodDefs mbase
       , knotMethodRefs = maybe HM.empty collectMethodRefs mbase
       , knotFields = maybe HM.empty collectFields mbase
       , knotTypes = maybe HM.empty collectTypes mbase
       , knotConstants = []
       , knotMaxId = 0
       }
  where
    collectClasses = foldr addClass HM.empty . dexClasses
    addClass klass = HM.insert (encodeType (classType klass)) klass
    collectFields = foldr addField HM.empty . dexFields
    addField f = HM.insert (encodeType (fieldClass f), fieldName f) f
    collectMethodRefs = foldr addMethodRef HM.empty . dexMethodRefs
    addMethodRef mref m =
      let key = (encodeType (methodRefClass mref), methodRefName mref, methodRefParameterTypes mref)
      in HM.insert key mref m
    collectMethodDefs = foldr addMethodDef HM.empty . dexMethods
    -- The keys for methods do not include the <this> parameter for
    -- instance methods or constructors because the MethodRefs never
    -- mention them.  The keys are generated from the method refs.
    addMethodDef m mp =
      let key = (encodeType (classType (methodClass m)), methodName m, methodTypeKey m)
      in HM.insert key m mp

methodTypeKey :: Method -> [Type]
methodTypeKey m
  | methodName m == "<init>" ||
    not (hasAccessFlag ACC_STATIC flags) =
      map parameterType $ tail (methodParameters m)
  | otherwise = map parameterType $ methodParameters m
  where
    flags = methodAccessFlags m

data KnotState =
  KnotState { knotIdSrc :: !Int
            , knotStringCache :: HashMap BS.ByteString Constant
            , knotIntCache :: Map Int64 Constant
            , knotClassConstantCache :: HashMap BS.ByteString Constant
            , knotStubs :: Maybe Stubs
            , knotDexFile :: DT.DexFile
              -- ^ This is the current Dex file being translated
            , knotDexFields :: !(Map DT.FieldId (BS.ByteString, BS.ByteString))
              -- ^ This MUST be reset after each dex file is
              -- processed.  It is only valid within the scope of a
              -- single dex.
            , knotDexMethods :: !(Map DT.MethodId (BS.ByteString, BS.ByteString, [Type]))
              -- ^ Likewise this one
            , knotDexTypes :: !(HashMap BS.ByteString Type)
              -- ^ This is the set of all types encountered so far, up
              -- to and including the current dex file.
            }

initialKnotState :: Maybe Stubs -> Maybe DexFile -> KnotState
initialKnotState mstubs mbase =
  KnotState { knotIdSrc = maybe 0 dexIdSrc mbase
            , knotDexFile = undefined
            , knotStringCache = strs -- HM.empty
            , knotIntCache = ints -- M.empty
            , knotClassConstantCache = klasses -- HM.empty
            , knotDexFields = M.empty
            , knotDexMethods = M.empty
            , knotDexTypes = maybe HM.empty collectTypes mbase
            , knotStubs = mstubs
            }
  where
    (strs, ints, klasses) = maybe (HM.empty, M.empty, HM.empty) collectConstants mbase
    collectConstants = foldr addConstant (HM.empty, M.empty, HM.empty) . dexConstants

collectTypes :: DexFile -> HashMap BS.ByteString Type
collectTypes = foldr addType HM.empty . dexTypes
  where
    addType t = HM.insert (encodeType t) t

addConstant :: Constant
            -> (HashMap BS.ByteString Constant, Map Int64 Constant, HashMap BS.ByteString Constant)
            -> (HashMap BS.ByteString Constant, Map Int64 Constant, HashMap BS.ByteString Constant)
addConstant k (strs, ints, klasses) =
  case k of
    ConstantInt _ i -> (strs, M.insert i k ints, klasses)
    ConstantString _ s -> (HM.insert s k strs, ints, klasses)
    ConstantClass _ t -> (strs, ints, HM.insert (encodeType t) k klasses)

-- | Translate types from bytestrings into a structured
-- representation.  Only add it if we haven't already seen it (we will
-- get duplicates between different dex files).
translateType :: (E.MonadThrow m)
                 => DT.DexFile
                 -> Knot
                 -> (DT.TypeId, DT.StringId)
                 -> m Knot
translateType df !m (tid, _) = do
  tname <- DT.getTypeName df tid
  case HM.member tname (knotTypes m) of
    True -> return m
    False -> do
      ty <- parseTypeName tname
      return m { knotTypes = HM.insert tname ty (knotTypes m) }

getStr' :: (E.MonadThrow m) => DT.StringId -> KnotMonad m BS.ByteString
getStr' sid
  | sid == -1 = error "Missing string bt"
  | otherwise = do
    df <- getDex
    DT.getStr df sid

lookupClass :: (E.MonadThrow m)
               => DT.TypeId
               -> KnotMonad m (Maybe Class)
lookupClass tid
  | tid == -1 = return Nothing
  | otherwise = do
    parentString <- getTypeName tid
    klasses <- asks knotClasses
    return $ HM.lookup parentString klasses

-- | Note: I don't think that the DT.TypeId here is actually the type
-- ID of the class...  DT.classId is accurate (and different).
translateClass :: (MonadFix m, E.MonadThrow m)
                  => Knot
                  -> (DT.TypeId, DT.Class)
                  -> KnotMonad m Knot
translateClass k (_, klass) = do
  cid <- freshId
  sname <- case DT.classSourceNameId klass of
    (-1) -> return "<stdin>"
    snid -> getStr' snid
  parent <- case DT.classSuperId klass of
    (-1) -> return Nothing
    sid -> liftM Just $ getTranslatedType sid
  t <- getTranslatedType (DT.classId klass)
  parentRef <- lookupClass (DT.classSuperId klass)
  staticFields <- mapM translateField (DT.classStaticFields klass)
  instanceFields <- mapM translateField (DT.classInstanceFields klass)
  itypes <- mapM getTranslatedType (DT.classInterfaces klass)

  (c, k2) <- mfix $ \tclass -> do
    (k1, directMethods) <- foldM (translateMethod (fst tclass)) (k, []) (DT.classDirectMethods klass)
    (k2, virtualMethods) <- foldM (translateMethod (fst tclass)) (k1, []) (DT.classVirtualMethods klass)

    let c = Class { classId = cid
                  , classType = t
                  , className = BS.pack $ PP.pretty t
                  , classSourceName = sname
                  , classAccessFlags = DT.classAccessFlags klass
                  , classParent = parent
                  , classParentReference = parentRef
                  , _classInterfaces = V.fromList itypes
                  , _classStaticFields = V.fromList staticFields
                  , _classInstanceFields = V.fromList instanceFields
                  , _classDirectMethods = V.fromList (reverse directMethods)
                  , _classVirtualMethods = V.fromList (reverse virtualMethods)
                  , _classStaticFieldMap = foldr addField HM.empty staticFields
                  , _classInstanceFieldMap = foldr addField HM.empty instanceFields
                  , _classMethodMap = foldr addMethod HM.empty (directMethods ++ virtualMethods)
                  }
    return (c, k2)

  classString <- getTypeName (DT.classId klass)
  -- See Note [Duplicate Class Definitions]
  case HM.member classString (knotClasses k2) of
    True -> return k2
    False -> return k2 { knotClasses = HM.insert classString c (knotClasses k2) }
  where
    addField (_, f) = HM.insert (fieldName f) f
    addMethod m = HM.insert (methodName m, methodSignature m) m

getDex :: (E.MonadThrow m) => KnotMonad m DT.DexFile
getDex = gets knotDexFile

getRawMethod' :: (E.MonadThrow m) => DT.MethodId -> KnotMonad m DT.Method
getRawMethod' mid = do
  df <- getDex
  lift $ DT.getMethod df mid

getRawProto' :: (E.MonadThrow m) => DT.ProtoId -> KnotMonad m DT.Proto
getRawProto' pid = do
  df <- getDex
  lift $ DT.getProto df pid

-- | This is a wrapper around the real @translateMethod@
-- ('translateMethod'') that checks to see if we have an override
-- stub.
translateMethod :: (MonadFix m, E.MonadThrow m)
                   => Class
                   -> (Knot, [Method])
                   -> DT.EncodedMethod
                   -> KnotMonad m (Knot, [Method])
translateMethod klass acc em = do
  s <- get
  let oldDex = knotDexFile s
  case knotStubs s of
    Nothing -> translateMethod' klass acc em
    Just stubs ->
      case St.matchingStub stubs oldDex em of
        Nothing -> translateMethod' klass acc em
        Just (df', em') -> do
          -- If we are replacing a method in the current dex file with
          -- a stub method, we need to *temporarily* swap out the dex
          -- file to the dex file for our stubs.  Swap it back when we
          -- are done translating this one method.
          put s { knotDexFile = df' }
          res <- translateMethod' klass acc em'
          modify $ \s' -> s' { knotDexFile = oldDex }
          return res

translateMethod' :: (MonadFix m, E.MonadThrow m)
                   => Class
                   -> (Knot, [Method])
                   -> DT.EncodedMethod
                   -> KnotMonad m (Knot, [Method])
translateMethod' klass (k, acc) em = do
  m <- getRawMethod' (DT.methId em)
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)

  df <- getDex

  paramList <- lift $ getParamList df em
  uid <- freshId

  tm <- mfix $ \tm -> do
    paramMap <- foldM (makeParameter tm) M.empty (zip [0..] paramList)

    (body, _) <- mfix $ \tiedKnot ->
      translateMethodBody df tm paramMap (snd tiedKnot) em


    let ps = M.elems paramMap
    return $ Method { methodId = uid
                    , methodName = mname
                    , methodReturnType = rt
                    , methodAccessFlags = DT.methAccessFlags em
                    , _methodParameters = V.fromList ps
                    , _methodBody = body
                    , methodClass = klass
                    }

  mrefs <- gets knotDexMethods
  let errMsg = E.throwM $ DT.NoMethodAtIndex (DT.methId em)
  stringKey <- maybe errMsg return $ M.lookup (DT.methId em) mrefs
  return (k { knotMethodDefs = HM.insert stringKey tm (knotMethodDefs k) }, tm : acc)

makeParameter :: (E.MonadThrow m)
                 => Method
                 -> Map Int Parameter
                 -> (Int, (Maybe BS.ByteString, DT.TypeId))
                 -> KnotMonad m (Map Int Parameter)
makeParameter tm m (ix, (name, tid)) = do
  pid <- freshId
  t <- getTranslatedType tid
  let p = Parameter { parameterId = pid
                    , parameterType = t
                    , parameterName = fromMaybe (generateNameForParameter ix) name
                    , parameterIndex = ix
                    , parameterMethod = tm
                    }
  return $ M.insert ix p m

translateMethodBody :: (MonadFix m, E.MonadThrow m)
                       => DT.DexFile
                       -> Method
                       -> Map Int Parameter
                       -> MethodKnot
                       -> DT.EncodedMethod
                       -> KnotMonad m (Maybe (V.Vector BasicBlock), MethodKnot)
translateMethodBody _ _ _ _ DT.EncodedMethod { DT.methCode = Nothing } = return (Nothing, emptyMethodKnot)
translateMethodBody df tm paramMap tiedMknot em = do
  labeling <- lift $ labelMethod df em
  let parameterLabels = labelingParameters labeling
      bbs = labelingBasicBlocks labeling
      blockList = basicBlocksAsList bbs
  mknot0 <- foldM addParameterLabel emptyMethodKnot parameterLabels
  (bs, resultKnot) <- foldM (translateBlock tm labeling tiedMknot) ([], mknot0) blockList
  return (Just (V.fromList (reverse bs)), resultKnot)
  where
    addParameterLabel mknot l@(ArgumentLabel _ ix) =
      case M.lookup ix paramMap of
        Nothing -> E.throwM $ DT.NoParameterAtIndex (DT.methId em) ix
        Just param ->
          return mknot { mknotValues = M.insert l (ParameterV param) (mknotValues mknot) }
    addParameterLabel _ l = E.throwM $ DT.NonArgumentLabelInParameterList (DT.methId em) (show l)

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
translateBlock :: (MonadFix m, E.MonadThrow m)
                  => Method
                  -> Labeling
                  -> MethodKnot
                  -> ([BasicBlock], MethodKnot)
                  -> (BlockNumber, Int, Vector DT.Instruction)
                  -> KnotMonad m ([BasicBlock], MethodKnot)
translateBlock tm labeling tiedMknot (bs, mknot) (bnum, indexStart, insts) = do
  bid <- freshId
  let blockPhis = M.findWithDefault [] bnum $ labelingBlockPhis labeling
      insts' = V.toList insts
  (b, mknot'') <- mfix $ \final -> do
    (phis, mknot') <- foldM (makePhi labeling tiedMknot (fst final)) ([], mknot) blockPhis
    (insns, mknot'') <- foldM (translateInstruction labeling tiedMknot bnum (fst final)) ([], mknot') (zip [indexStart..] insts')
    let blk = BasicBlock { basicBlockId = bid
                         , basicBlockNumber = bnum
                         , _basicBlockInstructions = V.fromList $ phis ++ reverse insns
                         , basicBlockPhiCount = length phis
                         , SSA._basicBlockSuccessors = V.fromList $ map (getFinalBlock tiedMknot) $ BB.basicBlockSuccessors bbs bnum
                         , SSA._basicBlockPredecessors = V.fromList $ map (getFinalBlock tiedMknot) $ BB.basicBlockPredecessors bbs bnum
                         , basicBlockMethod = tm
                         }
    return (blk, mknot'')
  return (b : bs, mknot'' { mknotBlocks = M.insert bnum b (mknotBlocks mknot'') })
  where
    bbs = labelingBasicBlocks labeling

-- FIXME: We could insert unconditional branches at the end of any
-- basic blocks that fallthrough without an explicit transfer
-- instruction...  That might not be useful, though, since there are
-- already implicit terminators in the presence of exceptions.

srcLabelForReg :: (E.MonadThrow m, FromRegister r)
                  => Labeling
                  -> Int
                  -> r
                  -> KnotMonad m Label
srcLabelForReg l ix r =
  maybe err return $ do
    regMap <- M.lookup ix (labelingReadRegs l)
    M.lookup (fromRegister r) regMap
  where
    err = E.throwM $ DT.NoLabelForExpectedRegister "source" (fromRegister r) ix

dstLabelForReg :: (E.MonadThrow m, FromRegister r)
                  => Labeling
                  -> Int
                  -> r
                  -> KnotMonad m Label
dstLabelForReg l ix r =
  maybe err return $ M.lookup ix (labelingWriteRegs l)
  where
    err = E.throwM $ DT.NoLabelForExpectedRegister "destination" (fromRegister r) ix

getFinalValue :: MethodKnot -> Label -> Value
getFinalValue mknot lbl =
  fromMaybe (error ("No value for label: " ++ show lbl)) $ M.lookup lbl (mknotValues mknot)

getFinalBlock :: MethodKnot -> BlockNumber -> BasicBlock
getFinalBlock mknot bnum =
  fromMaybe (error ("No basic block: " ++ show bnum)) $ M.lookup bnum (mknotBlocks mknot)

translateInstruction :: forall m . (E.MonadThrow m)
                        => Labeling
                        -> MethodKnot
                        -> BlockNumber
                        -> BasicBlock
                        -> ([Instruction], MethodKnot)
                        -> (Int, DT.Instruction)
                        -> KnotMonad m ([Instruction], MethodKnot)
translateInstruction labeling tiedMknot bnum bb acc@(insns, mknot) (instIndex, inst) =
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
                            , instructionBasicBlock = bb
                            }
      return (e : insns, addInstMapping mknot lbl e)
    DT.Move1 _ _ -> return acc
    DT.ReturnVoid -> do
      rid <- freshId
      let r = Return { instructionId = rid
                     , instructionType = VoidType
                     , instructionBasicBlock = bb
                     , returnValue = Nothing
                     }
      return (r : insns, mknot)
    DT.Return _ src -> do
      rid <- freshId
      lbl <- srcLabel src
      let r = Return { instructionId = rid
                     , instructionType = VoidType
                     , instructionBasicBlock = bb
                     , returnValue = Just $ getFinalValue tiedMknot lbl
                     }
      return (r : insns, mknot)
    DT.MonitorEnter src -> do
      mid <- freshId
      lbl <- srcLabel src
      let m = MonitorEnter { instructionId = mid
                           , instructionType = VoidType
                           , instructionBasicBlock = bb
                           , monitorReference = getFinalValue tiedMknot lbl
                           }
      return (m : insns, mknot)
    DT.MonitorExit src -> do
      mid <- freshId
      lbl <- srcLabel src
      let m = MonitorExit { instructionId = mid
                          , instructionType = VoidType
                          , instructionBasicBlock = bb
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
                        , instructionBasicBlock = bb
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
                         , instructionType = BooleanType
                         , instructionBasicBlock = bb
                         , instanceOfReference = getFinalValue tiedMknot srcLbl
                         , instanceOfType = t
                         }
      return (i : insns, addInstMapping mknot dstLbl i)
    DT.ArrayLength dst src -> do
      aid <- freshId
      srcLbl <- srcLabel src
      dstLbl <- dstLabel dst
      let a = ArrayLength { instructionId = aid
                          , instructionType = IntType
                          , instructionBasicBlock = bb
                          , arrayReference = getFinalValue tiedMknot srcLbl
                          }
      return (a : insns, addInstMapping mknot dstLbl a)
    DT.NewInstance dst tid -> do
      nid <- freshId
      dstLbl <- dstLabel dst
      t <- getTranslatedType tid
      let n = NewInstance { instructionId = nid
                          , instructionType = t
                          , instructionBasicBlock = bb
                          }
      return (n : insns, addInstMapping mknot dstLbl n)
    DT.NewArray dst src tid -> do
      nid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src
      t <- getTranslatedType tid
      let n = NewArray { instructionId = nid
                       , instructionType = t
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                            , instructionBasicBlock = bb
                            , fillArrayReference = getFinalValue tiedMknot srcLbl
                            , fillArrayContents = numbers
                            }
          in return (a : insns, mknot)
        _ -> E.throwM $ DT.NoArrayDataForFillArray instIndex
    DT.Throw src -> do
      tid <- freshId
      srcLbl <- srcLabel src
      let t = Throw { instructionId = tid
                    , instructionType = VoidType
                    , instructionBasicBlock = bb
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
                      , instructionBasicBlock = bb
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
                           , instructionBasicBlock = bb
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
                            , instructionBasicBlock = bb
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
                              , instructionBasicBlock = bb
                              , instanceOpReference = getFinalValue tiedMknot refLbl
                              , instanceOpField = f
                              , instanceOpPutValue = getFinalValue tiedMknot valLbl
                              }
          return (i : insns, mknot)
        DT.Get _ -> do
          dstLbl <- dstLabel dstOrSrc
          let i = InstanceGet { instructionId = iid
                              , instructionType = fieldType f
                              , instructionBasicBlock = bb
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
                            , instructionBasicBlock = bb
                            , staticOpField = f
                            , staticOpPutValue = getFinalValue tiedMknot valLbl
                            }
          return (s : insns, mknot)
        DT.Get _ -> do
          dstLbl <- dstLabel dstOrSrc
          let s = StaticGet { instructionId = sid
                            , instructionType = fieldType f
                            , instructionBasicBlock = bb
                            , staticOpField = f
                            }
          return (s : insns, addInstMapping mknot dstLbl s)
    DT.Unop op dst src -> do
      oid <- freshId
      dstLbl <- dstLabel dst
      srcLbl <- srcLabel src
      let o = UnaryOp { instructionId = oid
                      , instructionType = unaryOpType op
                      , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                       , instructionBasicBlock = bb
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
                                  , instructionBasicBlock = bb
                                  , branchTarget = getFinalBlock tiedMknot targetBlock
                                  }
      return (b : insns, mknot)
    DT.Goto16 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = UnconditionalBranch { instructionId = bid
                                  , instructionType = VoidType
                                  , instructionBasicBlock = bb
                                  , branchTarget = getFinalBlock tiedMknot targetBlock
                                  }
      return (b : insns, mknot)
    DT.Goto32 _ -> do
      bid <- freshId
      let [(Unconditional, targetBlock)] = basicBlockBranchTargets bbs bnum
          b = UnconditionalBranch { instructionId = bid
                                  , instructionType = VoidType
                                  , instructionBasicBlock = bb
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
                     , instructionBasicBlock = bb
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
                     , instructionBasicBlock = bb
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
                                , instructionBasicBlock = bb
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
                                , instructionBasicBlock = bb
                                , branchOperand1 = getFinalValue tiedMknot srcLbl
                                , branchOperand2 = zero
                                , branchTestType = op
                                , branchTarget = getFinalBlock tiedMknot ct
                                , branchFallthrough = getFinalBlock tiedMknot ft
                                }
      return (b : insns, mknot)
    DT.Invoke ikind _isVarArg mId argRegs -> do
      df <- getDex
      argRegs' <- lift $ filterWidePairs df mId ikind argRegs
      srcLbls <- mapM srcLabel argRegs'
      case ikind of
        DT.Virtual -> translateVirtualInvoke MethodInvokeVirtual mId srcLbls
        DT.Super -> translateVirtualInvoke MethodInvokeSuper mId srcLbls
        DT.Interface -> translateVirtualInvoke MethodInvokeInterface mId srcLbls
        DT.Direct -> translateDirectInvoke MethodInvokeDirect mId srcLbls
        DT.Static -> translateDirectInvoke MethodInvokeStatic mId srcLbls
  where
    dstLabel :: (E.MonadThrow m, FromRegister r) => r -> KnotMonad m Label
    dstLabel = dstLabelForReg labeling instIndex
    srcLabel :: (E.MonadThrow m, FromRegister r) => r -> KnotMonad m Label
    srcLabel = srcLabelForReg labeling instIndex
    bbs = labelingBasicBlocks labeling
    translateVirtualInvoke ikind mid argLbls = do
      iid <- freshId
      mref <- getMethodRef mid
      case NE.nonEmpty argLbls of
        Nothing -> E.throwM $ DT.NoReceiverForVirtualCall (show inst)
        Just nonEmptyLbls -> do
          let i = InvokeVirtual { instructionId = iid
                                , instructionType = methodRefReturnType mref
                                , instructionBasicBlock = bb
                                , invokeVirtualKind = ikind
                                , invokeVirtualMethod = mref
                                , invokeVirtualArguments = fmap (getFinalValue tiedMknot) nonEmptyLbls
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
                           , instructionBasicBlock = bb
                           , invokeDirectKind = ikind
                           , invokeDirectMethod = mref
                           , invokeDirectMethodDef = mdef
                           , invokeDirectArguments = map (getFinalValue tiedMknot) argLbls
                           }
      possibleDestination <- resultSavedAs labeling instIndex
      case possibleDestination of
        Nothing -> return (i : insns, mknot)
        Just dstLbl -> return (i : insns, addInstMapping mknot dstLbl i)



-- | Look up the exception handled in the given block, if any.  If the
-- block is not listed in a handler descriptor, that means this is a
-- finally block.  All we know in that case is that we have some
-- Throwable.
typeOfHandledException :: (E.MonadThrow m) => Labeling -> BlockNumber -> KnotMonad m Type
typeOfHandledException labeling bnum =
  case basicBlockHandlesException (labelingBasicBlocks labeling) bnum of
    Just exname -> parseTypeName exname
    Nothing -> parseTypeName "Ljava/lang/Throwable;"

isFallthroughEdge :: (JumpCondition, BlockNumber) -> Bool
isFallthroughEdge (Fallthrough, _) = True
isFallthroughEdge _ = False

getConstant :: (E.MonadThrow m) => DT.ConstArg -> KnotMonad m Value
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
      ccache <- gets knotClassConstantCache
      klassName <- getTypeName tid
      case HM.lookup klassName ccache of
        Just v -> return (ConstantV v)
        Nothing -> do
          cid <- freshId
          t <- getTranslatedType tid
          let c = ConstantClass cid t
          modify $ \s -> s { knotClassConstantCache = HM.insert klassName c (knotClassConstantCache s) }
          return (ConstantV c)

getConstantString :: (E.MonadThrow m) => DT.StringId -> KnotMonad m Value
getConstantString sid = do
  scache <- gets knotStringCache
  str <- getStr' sid
  case HM.lookup str scache of
    Just v -> return (ConstantV v)
    Nothing -> do
      cid <- freshId
      let c = ConstantString cid str
      modify $ \s -> s { knotStringCache = HM.insert str c (knotStringCache s) }
      return (ConstantV c)

getConstantInt :: (E.MonadThrow m, Integral n) => n -> KnotMonad m Value
getConstantInt (fromIntegral -> i) = do
  icache <- gets knotIntCache
  case M.lookup i icache of
    Just v -> return (ConstantV v)
    Nothing -> do
      cid <- freshId
      let c = ConstantInt cid i
      modify $ \s -> s { knotIntCache = M.insert i c (knotIntCache s) }
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
resultSavedAs :: (E.MonadThrow m) => Labeling -> Int -> KnotMonad m (Maybe Label)
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
makePhi :: (E.MonadThrow m)
           => Labeling
           -> MethodKnot
           -> BasicBlock
           -> ([Instruction], MethodKnot)
           -> Label
           -> KnotMonad m ([Instruction], MethodKnot)
makePhi labeling tiedMknot bb (insns, mknot) lbl@(PhiLabel _ _ _) = do
  phiId <- freshId
  let ivs = labelingPhiIncomingValues labeling lbl
      p = Phi { instructionId = phiId
              , instructionType = UnknownType
              , instructionBasicBlock = bb
              , phiValues = map labelToIncoming ivs
              }
  return (p : insns, mknot { mknotValues = M.insert lbl (InstructionV p) (mknotValues mknot) })
  where
    labelToIncoming (incBlock, incLbl) =
      (fromMaybe (error ("No block for incoming block id: " ++ show incBlock)) $ M.lookup incBlock (mknotBlocks tiedMknot),
       fromMaybe (error ("No value for incoming value: " ++ show incLbl)) $ M.lookup incLbl (mknotValues tiedMknot))
makePhi _ _ _ _ lbl = E.throwM $ DT.NonPhiLabelInBlockHeader $ show lbl

getTypeName :: (E.MonadThrow m) => DT.TypeId -> KnotMonad m BS.ByteString
getTypeName tid = do
  df <- getDex
  DT.getTypeName df tid


-- | We do not consult the tied knot for types since we can translate
-- them all up-front.
getTranslatedType :: (E.MonadThrow m) => DT.TypeId -> KnotMonad m Type
getTranslatedType tid = do
  ts <- gets knotDexTypes
  tname <- getTypeName tid
  case HM.lookup tname ts of
    Nothing -> E.throwM $ DT.NoTypeAtIndex tid
    Just t -> return t

getTranslatedField :: (E.MonadThrow m) => DT.FieldId -> KnotMonad m Field
getTranslatedField fid = do
  frefs <- gets knotDexFields
  case M.lookup fid frefs of
    Nothing -> E.throwM $ DT.NoFieldAtIndex fid
    Just stringKey -> do
      fs <- asks knotFields
      let errMsg = error ("No field for field id " ++ show fid)
      return $ fromMaybe errMsg $ HM.lookup stringKey fs

getTranslatedMethod :: (E.MonadThrow m) => DT.MethodId -> KnotMonad m (Maybe Method)
getTranslatedMethod mid = do
  mrefs <- gets knotDexMethods
  case M.lookup mid mrefs of
    Nothing -> E.throwM $ DT.NoMethodAtIndex mid
    Just stringKey -> do
      ms <- asks knotMethodDefs
      return $ HM.lookup stringKey ms

-- | Translate an entry from the DexFile fields map.  These do not
-- contain access flags, but have everything else.
translateFieldRef :: (E.MonadThrow m)
                     => Knot
                     -> (DT.FieldId, DT.Field)
                     -> KnotMonad m Knot
translateFieldRef !knot (fid, f) = do
  fname <- getStr' (DT.fieldNameId f)
  cname <- getTypeName (DT.fieldClassId f)
  ftype <- getTranslatedType (DT.fieldTypeId f)
  klass <- getTranslatedType (DT.fieldClassId f)
  uid <- freshId
  let fld = Field { fieldId = uid
                  , fieldName = fname
                  , fieldType = ftype
                  , fieldClass = klass
                  }
      stringKey = (cname, fname)
  modify $ \(!s) -> s { knotDexFields = M.insert fid stringKey (knotDexFields s) }
  -- We may already have a definition of a field in the knot due to an
  -- input DexFile.  We don't want to overwrite it.
  case HM.member stringKey (knotFields knot) of
    True -> return knot
    False -> return knot { knotFields = HM.insert stringKey fld (knotFields knot) }

translateMethodRef :: (E.MonadThrow m)
                      => Knot
                      -> (DT.MethodId, DT.Method)
                      -> KnotMonad m Knot
translateMethodRef !knot (mid, m) = do
  proto <- getRawProto' (DT.methProtoId m)
  mname <- getStr' (DT.methNameId m)
  rt <- getTranslatedType (DT.protoRet proto)
  cid <- getTranslatedType (DT.methClassId m)
  ptypes <- mapM getTranslatedType (DT.protoParams proto)

  cname <- getTypeName (DT.methClassId m)

  uid <- freshId

  let mref = MethodRef { methodRefId = uid
                       , methodRefClass = cid
                       , methodRefName = mname
                       , methodRefReturnType = rt
                       , methodRefParameterTypes = ptypes
                       }
      stringKey = (cname, mname, ptypes)
  modify $ \(!s) -> s { knotDexMethods = M.insert mid stringKey (knotDexMethods s) }
  case HM.member stringKey (knotMethodRefs knot) of
    True -> return knot
    False -> return knot { knotMethodRefs = HM.insert stringKey mref (knotMethodRefs knot) }

translateField :: (E.MonadThrow m) => DT.EncodedField -> KnotMonad m (DT.AccessFlags, Field)
translateField ef = do
  dfs <- gets knotDexFields
  case M.lookup (DT.fieldId ef) dfs of
    Nothing -> E.throwM $ DT.NoFieldAtIndex (DT.fieldId ef)
    Just stringKey -> do
      fs <- asks knotFields
      let errMsg = error ("No field for index " ++ show (DT.fieldId ef))
      return . (DT.fieldAccessFlags ef,) $ fromMaybe errMsg $ HM.lookup stringKey fs

-- | Allocate a fresh globally unique (within a single Dex file)
-- identifier.
freshId :: (E.MonadThrow m) => KnotMonad m Int
freshId = do
  s <- get
  put s { knotIdSrc = knotIdSrc s + 1 }
  return $ knotIdSrc s


dexMethods :: DexFile -> [Method]
dexMethods df = [ m
                | k <- dexClasses df
                , m <- classDirectMethods k ++ classVirtualMethods k
                ]

{- Note [Translation]

Before building up the SSA-based IR, we label every Value with its
local SSA number using the algorithm from

  http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf

This is fairly different from the Cytron algorithm.  It works
backwards instead of forwards and does not require a dominance
frontier or a full CFG.  Once each value is identified this way,
making an SSA value for it should be simpler.

-}

{- Note [Synthesized Types]

We record a table of all of the types in the DexFile.  The raw dex
file from an Apk has a similar table, however ours has more entries.
The raw type table only includes types mentioned by name in the dalvik
source.  Our SSA transformation "synthesizes" new types for certain
instructions (e.g., array-get, which shares a type with the inner type
of the array being operated on).  If these synthetic types do not
appear in the original dalvik, they would not have appeared in our
table.  We use a post-processing pass over the instruction stream to
pick up all of our synthesized types.

-}

{- Note [Duplicate Class Definitions]

When a DEX file defines a class that is also defined in the Android
framework (usually represented in this library as the _base_ DEX
file), we prefer the version from the _base_.  This is the secure
choice from the perspective of the Android platform and is
corroborated by some documentation:

 * https://android.googlesource.com/platform/dalvik.git/+/9735865121118f255c4c15183b236f9f30e795f1/vm/oo/Class.cpp

 * http://paulononaka.wordpress.com/2011/07/02/how-to-install-a-application-in-background-on-android/

If users could provide alternative implementations of framework
classes, they could trivially bypass permission checks.  Duplicate
definitions appear in the wild, for some reason.  One reason,
discussed in the second link above, seems to be related to accessing
private functions in the framework.  The framework contains many
classes required for it to run that are not available to users.  It
can all be accessed via reflection, but that approach is not very
convenient.  Instead, people provide no-op stubs exposing the same
classes and methods linked into their own applications.  The dalvik
loader just ignores the duplicates in the application and links the
calls to the private framework methods.

-}
