{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
-- | Support serializing Dalvik code in SSA form.
--
-- This is binary serialization and it preserves sharing.  Explicit
-- functions are provided (as opposed to class instances) for two
-- reasons:
--
-- 1) Decoupling from a specific serialization library
--
-- 2) Some trickiness was required to deal with knot tying during
-- deserialization.  The trick did not fit in the instance declaration
-- cleanly.
module Dalvik.SSA.Internal.Serialize (
  deserializeDex,
  serializeDex,
  -- Helpers
  dexFields,
  dexMethodRefs
  ) where

import Control.Applicative
import Control.Arrow ( first, second )
import Control.Monad ( unless )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NEL
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Monoid
import qualified Data.Serialize as S
import qualified Data.Vector as V

import Dalvik.SSA.Internal.Pretty ()
import Dalvik.SSA.Types

formatVersion :: Int
formatVersion = 1

-- | This file header is a clone of the PNG header, but with a
-- different ASCII tag.
fileHeader :: BS.ByteString
fileHeader = LBS.toStrict $ LBS.toLazyByteString bldr
  where
    bldr = mconcat [ LBS.word8 0x89
                   , LBS.string8 "DLVK"
                   , LBS.word8 0x0d
                   , LBS.word8 0x0a
                   , LBS.word8 0x1a
                   , LBS.word8 0x0a
                   ]

-- | Deserialize a previously serialized Dex file from a strict 'ByteString'
--
-- This operation preserves the sharing of the original dex file.
deserializeDex :: BS.ByteString -> Either String DexFile
deserializeDex bs =
  let getKnot = snd . either error id
      res = S.runGet (getDex (getKnot res)) bs
  in case res of
    Left err -> Left err
    Right (df, _) -> Right df

serializeDex :: DexFile -> BS.ByteString
serializeDex = S.runPut . putDex

-- | A type holding state for the knot tying procedure.
data Knot = Knot { knotTypeTable :: !(IntMap Type)
                 , knotClasses :: !(IntMap Class)
                 , knotMethods :: !(IntMap Method)
                 , knotValues :: !(IntMap Value)
                 , knotBlocks :: !(IntMap BasicBlock)
                 , knotMethodRefs :: !(IntMap MethodRef)
                 , knotFields :: !(IntMap Field)
                 }

emptyKnot :: Knot
emptyKnot = Knot { knotTypeTable = IM.empty
                 , knotClasses = IM.empty
                 , knotMethods = IM.empty
                 , knotValues = IM.empty
                 , knotBlocks = IM.empty
                 , knotMethodRefs = IM.empty
                 , knotFields = IM.empty
                 }

-- | Format:
--
-- Int (id source)
-- Type list
-- Constant list
-- Field list
-- Method ref list
-- Class list
putDex :: DexFile -> S.Put
putDex df = do
  S.putByteString fileHeader
  S.put formatVersion
  S.put (dexIdSrc df)
  tt <- putTypeTable (dexTypes df)
  putList (putConstant tt) (dexConstants df)
  putList (putField tt) (dexFields df)
  putList (putMethodRef tt) (dexMethodRefs df)
  putList (putClass tt) (dexClasses df)
  putList (putAnnotations tt) (map (first classId) (HM.toList (_dexClassAnnotations df)))
  putList (putAnnotations tt) (map (first methodRefId . snd) (HM.toList (_dexMethodAnnotations df)))

getDex :: Knot -> S.Get (DexFile, Knot)
getDex fknot = do
  sig <- S.getByteString $ fromIntegral $ BS.length fileHeader
  unless (sig == fileHeader) $ fail ("Deserialize: Signature mismatch, got " ++ show sig)
  fv <- S.get
  unless (fv == formatVersion) $ fail ("Deserialize: File format mismatch, got " ++ show fv ++ " but was expecting " ++ show formatVersion)
  idSrc <- S.get
  tt <- getTypeTable
  constants <- getList (getConstant tt)
  let knot0 = emptyKnot { knotTypeTable = tt
                        , knotValues = foldr (\c -> IM.insert (constantId c) (toValue c)) IM.empty constants
                        }
  -- We don't need the method refs here, we just need to populate the
  -- state with them
  (_, knot1) <- getListAccum getField knot0
  (_, knot2) <- getListAccum getMethodRef knot1
  (classes, knot3) <- getListAccum (getClass fknot) knot2
  let cache = foldr (\klass -> HM.insert (classType klass) klass) HM.empty classes
      ncache = foldr (\klass -> HM.insert (className klass) klass) HM.empty classes

  -- We added support for annotations after the rest of the
  -- serialization infrastructure was in place.  Some existing data
  -- files exist without annotations; this check lets us safely
  -- deserialize them (without annotations, of course).
  isE <- S.isEmpty
  (classAnnots, methodAnnots) <- case isE of
    False -> do
      classAnnots <- getList (getClassAnnotation knot3)
      methodAnnots <- getList (getMethodAnnotation knot3)
      return (classAnnots, methodAnnots)
    True -> return ([], [])
  return (DexFile { _dexClasses = V.fromList classes
                  , _dexConstants = V.fromList constants
                  , _dexTypes = V.fromList (IM.elems tt)
                  , dexIdSrc = idSrc
                  , _dexClassesByType = cache
                  , _dexClassesByName = ncache
                  , _dexClassAnnotations = HM.fromList classAnnots
                  , _dexMethodAnnotations = HM.fromList [ (mkMethodRefKey mref, (mref, annots))
                                                        | (mref, annots) <- methodAnnots
                                                        ]
                  }, knot3)
  where
    mkMethodRefKey mref = (methodRefClass mref, methodRefName mref, methodRefParameterTypes mref)

putAnnotations :: Map Type Int -> (UniqueId, [VisibleAnnotation]) -> S.Put
putAnnotations tt (cid, annots) = do
  S.put cid
  putList putVisibleAnnotation annots
  where
    putVisibleAnnotation :: VisibleAnnotation -> S.Put
    putVisibleAnnotation va = do
      putAnnotationVisibility (annotationVisibility va)
      putAnnotation tt (annotationValue va)

putAnnotation :: Map Type Int -> Annotation -> S.Put
putAnnotation tt a = do
  putType tt (annotationType a)
  putList putAnnotationArgument (annotationArguments a)
  where
    putAnnotationArgument (name, val) = do
      S.put name
      putAnnotationValue tt val

getAnnotation :: Knot -> S.Get Annotation
getAnnotation k0 = do
  t <- getType (knotTypeTable k0)
  args <- getList getAnnotationArgument
  return Annotation { annotationType = t
                    , annotationArguments = args
                    }
  where
    getAnnotationArgument = do
      name <- S.get
      v <- getAnnotationValue k0
      return (name, v)

putAnnotationValue :: Map Type Int -> AnnotationValue -> S.Put
putAnnotationValue tt val =
  case val of
    AVInt i -> S.putWord8 0 >> S.put i
    AVChar c -> S.putWord8 1 >> S.put c
    AVFloat f -> S.putWord8 2 >> S.put f
    AVDouble d -> S.putWord8 3 >> S.put d
    AVString s -> S.putWord8 4 >> S.put s
    AVType t -> S.putWord8 5 >> putType tt t
    AVField f -> S.putWord8 6 >> S.put (fieldId f)
    AVMethod m -> S.putWord8 7 >> S.put (methodRefId m)
    AVEnum f -> S.putWord8 8 >> S.put (fieldId f)
    AVArray vs -> S.putWord8 9 >> putList (putAnnotationValue tt) vs
    AVAnnotation a -> S.putWord8 10 >> putAnnotation tt a
    AVNull -> S.putWord8 11
    AVBool b -> S.putWord8 12 >> S.put b

getAnnotationValue :: Knot -> S.Get AnnotationValue
getAnnotationValue k = do
  tag <- S.getWord8
  case tag of
    0 -> AVInt <$> S.get
    1 -> AVChar <$> S.get
    2 -> AVFloat <$> S.get
    3 -> AVDouble <$> S.get
    4 -> AVString <$> S.get
    5 -> AVType <$> getType (knotTypeTable k)
    6 -> AVField <$> getField' k
    7 -> AVMethod <$> getMethodRef' k
    8 -> AVEnum <$> getField' k
    9 -> AVArray <$> getList (getAnnotationValue k)
    10 -> AVAnnotation <$> getAnnotation k
    11 -> return AVNull
    12 -> AVBool <$> S.get
    _ -> error ("Unknown annotation value tag while deserializing: " ++ show tag)

putAnnotationVisibility :: AnnotationVisibility -> S.Put
putAnnotationVisibility v =
  case v of
    AVBuild -> S.putWord8 0
    AVRuntime -> S.putWord8 1
    AVSystem -> S.putWord8 2

getAnnotationVisibility :: S.Get AnnotationVisibility
getAnnotationVisibility = do
  b <- S.getWord8
  case b of
    0 -> return AVBuild
    1 -> return AVRuntime
    2 -> return AVSystem
    _ -> error ("Invalid annotation visibility while deserializing: " ++ show b)

getClassAnnotation :: Knot -> S.Get (Class, [VisibleAnnotation])
getClassAnnotation k0 = do
  cls <- getClass' k0
  annots <- getList (getVisibleAnnotation k0)
  return (cls, annots)

getMethodAnnotation :: Knot -> S.Get (MethodRef, [VisibleAnnotation])
getMethodAnnotation k0 = do
  m <- getMethodRef' k0
  annots <- getList (getVisibleAnnotation k0)
  return (m, annots)

getVisibleAnnotation :: Knot -> S.Get VisibleAnnotation
getVisibleAnnotation k0 = do
  vis <- getAnnotationVisibility
  a <- getAnnotation k0
  return VisibleAnnotation { annotationVisibility = vis
                           , annotationValue = a
                           }

getTypeTable :: S.Get (IntMap Type)
getTypeTable = do
  lst <- S.get
  return $ foldr (\(i, t) -> IM.insert i t) IM.empty lst

putTypeTable :: [Type] -> S.PutM (Map Type Int)
putTypeTable (zip [0..] -> ts) = do
  let m = foldr (\(i, t) -> M.insert t i) M.empty ts
  S.put ts
  return m

putConstant :: Map Type Int -> Constant -> S.Put
putConstant tt c =
  case c of
    ConstantInt uid i -> do
      S.putWord8 0
      S.put uid
      S.put i
    ConstantString uid s -> do
      S.putWord8 1
      S.put uid
      S.put s
    ConstantClass uid t -> do
      S.putWord8 2
      S.put uid
      putType tt t

getConstant :: IntMap Type -> S.Get Constant
getConstant tt = do
  tag <- S.getWord8
  case tag of
    0 -> ConstantInt <$> S.get <*> S.get
    1 -> ConstantString <$> S.get <*> S.get
    2 -> do
      uid <- S.get
      t <- getType tt
      return $ ConstantClass uid t
    _ -> error ("Deserializing invalid constant tag: " ++ show tag)

putType :: Map Type Int -> Type -> S.Put
putType tt t =
  case M.lookup t tt of
    Just tid -> S.put tid
    Nothing -> error ("Serializing type with no id: " ++ show t)

getType :: IntMap Type -> S.Get Type
getType tt = do
  tid <- S.get
  case IM.lookup tid tt of
    Just t -> return t
    Nothing -> error ("Deserializing type with unknown id: " ++ show tid)

getField' :: Knot -> S.Get Field
getField' k0 = do
  fid <- S.get
  let flds = knotFields k0
  case IM.lookup fid (knotFields k0) of
    Nothing -> error ("Deserializing field with unknown id: " ++ show (fid, IM.findMin flds, IM.findMax flds, IM.size flds))
    Just f -> return f

getClass' :: Knot -> S.Get Class
getClass' k0 = do
  cid <- S.get
  case IM.lookup cid (knotClasses k0) of
    Nothing -> error ("Deserializing class with unknown id: " ++ show cid)
    Just cls -> return cls

getMethodRef' :: Knot -> S.Get MethodRef
getMethodRef' k = do
  mid <- S.get
  case IM.lookup mid (knotMethodRefs k) of
    Nothing -> error ("Deserializing unknown method ref id: " ++ show mid)
    Just mref -> return mref

putClass :: Map Type Int -> Class -> S.Put
putClass tt klass = do
  S.put (classId klass)
  putType tt (classType klass)
  S.put (className klass)
  S.put (classSourceName klass)
  S.put (classAccessFlags klass)
  S.putMaybeOf (putType tt) (classParent klass)
  S.putMaybeOf S.put (fmap classId (classParentReference klass))
  S.putListOf (putType tt) (classInterfaces klass)
  putList (putMethod tt) (classDirectMethods klass)
  putList (putMethod tt) (classVirtualMethods klass)
  putList putAccessField (classStaticFields klass)
  putList putAccessField (classInstanceFields klass)

getClass :: Knot -> Knot -> S.Get (Class, Knot)
getClass fknot k0 = do
  let tt = knotTypeTable k0
  cid <- S.get
  ct <- getType tt
  name <- S.get
  sourceName <- S.get
  flags <- S.get
  parent <- S.getMaybeOf (getType tt)
  mparentRefId <- S.get
  let classErr = error ("Unknown class parent while decoding " ++ show cid)
      parentRef = fmap (\p -> fromMaybe classErr $ IM.lookup p (knotClasses fknot)) mparentRefId
  ifaces <- S.getListOf (getType tt)
  (dms, k1) <- getListAccum (getMethod fknot) k0
  (vms, k2) <- getListAccum (getMethod fknot) k1
  sfields <- getList (getAccessField fknot)
  ifields <- getList (getAccessField fknot)
  let klass = Class { classId = cid
                    , classType = ct
                    , className = name
                    , classSourceName = sourceName
                    , classAccessFlags = flags
                    , classParent = parent
                    , classParentReference = parentRef
                    , _classInterfaces = V.fromList ifaces
                    , _classDirectMethods = V.fromList dms
                    , _classVirtualMethods = V.fromList vms
                    , _classStaticFields = V.fromList sfields
                    , _classInstanceFields = V.fromList ifields
                    , _classStaticFieldMap = indexFields sfields
                    , _classInstanceFieldMap = indexFields ifields
                    , _classMethodMap = indexMethods (vms ++ dms)
                    }
      k3 = k2 { knotClasses = IM.insert cid klass (knotClasses k2) }
  return (klass, k3)
  where
    indexFields = foldr (\(_, f) -> HM.insert (fieldName f) f) HM.empty
    indexMethods = foldr (\m -> HM.insert (methodName m, methodSignature m) m) HM.empty

putAccessField :: (AccessFlags, Field) -> S.Put
putAccessField (flags, f) = S.put flags >> S.put (fieldId f)

getAccessField :: Knot -> S.Get (AccessFlags, Field)
getAccessField fknot = do
  flags <- S.get
  fid <- S.get
  let errMsg = error ("No field for id " ++ show fid)
      f = fromMaybe errMsg $ IM.lookup fid (knotFields fknot)
  return (flags, f)

putField :: Map Type Int -> Field -> S.Put
putField tt f = do
  S.put (fieldId f)
  S.put (fieldName f)
  putType tt (fieldType f)
  putType tt (fieldClass f)

getField :: Knot -> S.Get (Field, Knot)
getField k0 = do
  fid <- S.get
  name <- S.get
  ft <- getType tt
  fc <- getType tt
  let f = Field { fieldId = fid
                , fieldName = name
                , fieldType = ft
                , fieldClass = fc
                }
      k1 = k0 { knotFields = IM.insert fid f (knotFields k0) }
  return (f, k1)
  where
    tt = knotTypeTable k0

putMethod :: Map Type Int -> Method -> S.Put
putMethod tt m = do
  S.put (methodId m)
  S.put (methodName m)
  putType tt (methodReturnType m)
  S.put (methodAccessFlags m)
  putList (putParameter tt) (methodParameters m)
  -- Encode the body as a list since that is a bit easier to decode.
  putList (putBlock tt) (fromMaybe [] (methodBody m))
  S.put (classId (methodClass m))

getMethod :: Knot -> Knot -> S.Get (Method, Knot)
getMethod fknot k0 = do
  mid <- S.get
  name <- S.get
  rt <- getType (knotTypeTable k0)
  flags <- S.get
  (ps, k1) <- getListAccum (getParameter fknot) k0
  (b, k2) <- getListAccum (getBlock fknot) k1
  cid <- S.get
  let errMsg = error ("No class " ++ show cid ++ " while decoding method " ++ show mid)
      klass = fromMaybe errMsg $ IM.lookup cid (knotClasses fknot)
      m = Method { methodId = mid
                 , methodName = name
                 , methodReturnType = rt
                 , methodAccessFlags = flags
                 , _methodParameters = V.fromList ps
                 , _methodBody = if null b then Nothing else Just (V.fromList b)
                 , methodClass = klass
                 }
      k3 = k2 { knotMethods = IM.insert mid m (knotMethods k2) }
  return (m, k3)

putParameter :: Map Type Int -> Parameter -> S.Put
putParameter tt p = do
  S.put (parameterId p)
  putType tt (parameterType p)
  S.put (parameterName p)
  S.put (parameterIndex p)
  S.put (methodId (parameterMethod p))

getParameter :: Knot -> Knot -> S.Get (Parameter, Knot)
getParameter fknot k = do
  pid <- S.get
  pt <- getType (knotTypeTable k)
  name <- S.get
  ix <- S.get
  mix <- S.get
  let errMsg = error ("No method " ++ show mix ++ " for parameter " ++ show pid)
      m = fromMaybe errMsg $ IM.lookup mix (knotMethods fknot)
      p = Parameter { parameterId = pid
                    , parameterType = pt
                    , parameterName = name
                    , parameterIndex = ix
                    , parameterMethod = m
                    }
      k' = k { knotValues = IM.insert pid (toValue p) (knotValues k) }
  return (p, k')

putBlock :: Map Type Int -> BasicBlock -> S.Put
putBlock tt b = do
  S.put (basicBlockId b)
  S.put (basicBlockNumber b)
  putList (putInstruction tt) (basicBlockInstructions b)
  S.put (basicBlockPhiCount b)
  S.put (map basicBlockId (basicBlockSuccessors b))
  S.put (map basicBlockId (basicBlockPredecessors b))
  S.put (methodId (basicBlockMethod b))

getBlock :: Knot -> Knot -> S.Get (BasicBlock, Knot)
getBlock fknot k0 = do
  bid <- S.get
  bnum <- S.get
  (is, k1) <- getListAccum (getInstruction fknot) k0
  phis <- S.get
  sids <- S.get
  pids <- S.get
  mid <- S.get
  let errMsg = error ("Could not find method " ++ show mid ++ " while decoding block " ++ show bid)
      m = fromMaybe errMsg $ IM.lookup mid (knotMethods fknot)
      berrMsg i = error ("Could not translate block id " ++ show i ++ " while decoding block " ++ show bid)
      fromBlockId i = fromMaybe (berrMsg i) $ IM.lookup i (knotBlocks fknot)
      b = BasicBlock { basicBlockId = bid
                     , basicBlockNumber = bnum
                     , _basicBlockInstructions = V.fromList is
                     , basicBlockPhiCount = phis
                     , _basicBlockSuccessors = V.fromList $ map fromBlockId sids
                     , _basicBlockPredecessors = V.fromList $ map fromBlockId pids
                     , basicBlockMethod = m
                     }
      k2 = k1 { knotBlocks = IM.insert bid b (knotBlocks k1) }
  return (b, k2)

-- | Output the common values for each instruction, followed by a tag,
-- followed by instruction-specific data.
putInstruction :: Map Type Int -> Instruction -> S.Put
putInstruction tt i = do
  S.put (instructionId i)
  putType tt (instructionType i)
  S.put (basicBlockId (instructionBasicBlock i))
  case i of
    Return { returnValue = rv } -> do
      S.putWord8 0
      S.put (fmap valueId rv)
    MoveException {} -> do
      S.putWord8 1
    MonitorEnter { monitorReference = r } -> do
      S.putWord8 2
      S.put (valueId r)
    MonitorExit { monitorReference = r } -> do
      S.putWord8 3
      S.put (valueId r)
    CheckCast { castReference = r, castType = t } -> do
      S.putWord8 4
      S.put (valueId r)
      putType tt t
    InstanceOf { instanceOfReference = r
               , instanceOfType = t
               } -> do
      S.putWord8 5
      S.put (valueId r)
      putType tt t
    ArrayLength { arrayReference = r } -> do
      S.putWord8 6
      S.put (valueId r)
    NewInstance {} -> do
      S.putWord8 7
    NewArray { newArrayLength = len, newArrayContents = vs } -> do
      S.putWord8 8
      S.put (valueId len)
      S.put (fmap (map valueId) vs)
    FillArray { fillArrayReference = r, fillArrayContents = is } -> do
      S.putWord8 9
      S.put (valueId r)
      S.put is
    Throw { throwReference = r } -> do
      S.putWord8 10
      S.put (valueId r)
    ConditionalBranch { branchOperand1 = op1
                      , branchOperand2 = op2
                      , branchTestType = ty
                      , branchTarget = dest
                      , branchFallthrough = ft
                      } -> do
      S.putWord8 11
      S.put (valueId op1)
      S.put (valueId op2)
      S.put ty
      S.put (basicBlockId dest)
      S.put (basicBlockId ft)
    UnconditionalBranch { branchTarget = bt } -> do
      S.putWord8 12
      S.put (basicBlockId bt)
    Switch { switchValue = sv
           , switchTargets = ts
           , switchFallthrough = ft
           } -> do
      S.putWord8 13
      S.put (valueId sv)
      S.put (map (second basicBlockId) ts)
      S.put (basicBlockId ft)
    Compare { compareOperation = op
            , compareOperand1 = op1
            , compareOperand2 = op2
            } -> do
      S.putWord8 14
      S.put op
      S.put (valueId op1)
      S.put (valueId op2)
    UnaryOp { unaryOperand = v
            , unaryOperation = op
            } -> do
      S.putWord8 15
      S.put (valueId v)
      S.put op
    BinaryOp { binaryOperand1 = op1
             , binaryOperand2 = op2
             , binaryOperation = op
             } -> do
      S.putWord8 16
      S.put (valueId op1)
      S.put (valueId op2)
      S.put op
    ArrayGet { arrayReference = r, arrayIndex = ix } -> do
      S.putWord8 17
      S.put (valueId r)
      S.put (valueId ix)
    ArrayPut { arrayReference = r
             , arrayIndex = ix
             , arrayPutValue = v
             } -> do
      S.putWord8 18
      S.put (valueId r)
      S.put (valueId ix)
      S.put (valueId v)
    StaticGet { staticOpField = f } -> do
      S.putWord8 19
      S.put (fieldId f)
    StaticPut { staticOpField = f, staticOpPutValue = v } -> do
      S.putWord8 20
      S.put (fieldId f)
      S.put (valueId v)
    InstanceGet { instanceOpReference = r, instanceOpField = f } -> do
      S.putWord8 21
      S.put (valueId r)
      S.put (fieldId f)
    InstancePut { instanceOpReference = r
                , instanceOpField = f
                , instanceOpPutValue = v
                } -> do
      S.putWord8 22
      S.put (valueId r)
      S.put (fieldId f)
      S.put (valueId v)
    InvokeVirtual { invokeVirtualKind = k
                  , invokeVirtualMethod = mref
                  , invokeVirtualArguments = args
                  } -> do
      S.putWord8 23
      S.put k
      S.put (methodRefId mref)
      S.put $ F.toList $ fmap valueId args
    InvokeDirect { invokeDirectKind = k
                 , invokeDirectMethod = mref
                 , invokeDirectMethodDef = mdef
                 , invokeDirectArguments = args
                 } -> do
      S.putWord8 24
      S.put k
      S.put (methodRefId mref)
      S.put (fmap methodId mdef)
      S.put (fmap valueId args)
    Phi { phiValues = ivs } -> do
      S.putWord8 25
      S.put (map (\(b, v) -> (basicBlockId b, valueId v)) ivs)

getInstruction :: Knot -> Knot -> S.Get (Instruction, Knot)
getInstruction fknot k0 = do
  iid <- S.get
  it <- getType (knotTypeTable k0)
  let blockErr b = error ("No block with id " ++ show b ++ " while decoding instruction " ++ show iid)
      toBlock b = fromMaybe (blockErr b) $ IM.lookup b (knotBlocks fknot)
      valueErr v = error ("No value with id " ++ show v ++ " while decoding instruction " ++ show iid)
      toVal v = fromMaybe (valueErr v) $ IM.lookup v (knotValues fknot)
      mrefErr m = error ("No method ref with id " ++ show m ++ " while decoding instruction " ++ show iid)
      toMethodRef m = fromMaybe (mrefErr m) $ IM.lookup m (knotMethodRefs fknot)
      getValue = toVal <$> S.get
      getBB = toBlock <$> S.get
      getMRef = toMethodRef <$> S.get
  bb <- getBB
  tag <- S.getWord8
  let fieldErr f = error ("No field with id " ++ show f ++ " while decoding instruction " ++ show iid ++ " tag " ++ show tag)
      toField f = fromMaybe (fieldErr f) $ IM.lookup f (knotFields fknot)
      getF = toField <$> S.get
  case tag of
    0 -> do
      mrvid <- S.get
      let i = Return { instructionId = iid
                     , instructionType = it
                     , instructionBasicBlock = bb
                     , returnValue = fmap toVal mrvid
                     }
      return (i, addInstruction i k0)
    1 -> do
      let i = MoveException { instructionId = iid
                            , instructionType = it
                            , instructionBasicBlock = bb
                            }
      return (i, addInstruction i k0)
    2 -> do
      r <- getValue
      let i = MonitorEnter { instructionId = iid
                           , instructionType = it
                           , instructionBasicBlock = bb
                           , monitorReference = r
                           }
      return (i, addInstruction i k0)
    3 -> do
      r <- getValue
      let i = MonitorExit { instructionId = iid
                          , instructionType = it
                          , instructionBasicBlock = bb
                          , monitorReference = r
                          }
      return (i, addInstruction i k0)
    4 -> do
      r <- getValue
      t <- getType tt
      let i = CheckCast { instructionId = iid
                        , instructionType = it
                        , instructionBasicBlock = bb
                        , castReference = r
                        , castType = t
                        }
      return (i, addInstruction i k0)
    5 -> do
      r <- getValue
      t <- getType tt
      let i = InstanceOf { instructionId = iid
                         , instructionType = it
                         , instructionBasicBlock = bb
                         , instanceOfReference = r
                         , instanceOfType = t
                         }
      return (i, addInstruction i k0)
    6 -> do
      r <- getValue
      let i = ArrayLength { instructionId = iid
                          , instructionType = it
                          , instructionBasicBlock = bb
                          , arrayReference = r
                          }
      return (i, addInstruction i k0)
    7 -> do
      let i = NewInstance { instructionId = iid
                          , instructionType = it
                          , instructionBasicBlock = bb
                          }
      return (i, addInstruction i k0)
    8 -> do
      len <- getValue
      cids <- S.get
      let i = NewArray { instructionId = iid
                       , instructionType = it
                       , instructionBasicBlock = bb
                       , newArrayLength = len
                       , newArrayContents = fmap (map toVal) cids
                       }
      return (i, addInstruction i k0)
    9 -> do
      r <- getValue
      contents <- S.get
      let i = FillArray { instructionId = iid
                        , instructionType = it
                        , instructionBasicBlock = bb
                        , fillArrayReference = r
                        , fillArrayContents = contents
                        }
      return (i, addInstruction i k0)
    10 -> do
      r <- getValue
      let i = Throw { instructionId = iid
                    , instructionType = it
                    , instructionBasicBlock = bb
                    , throwReference = r
                    }
      return (i, addInstruction i k0)
    11 -> do
      op1 <- getValue
      op2 <- getValue
      test <- S.get
      dest <- getBB
      fallthrough <- getBB
      let i = ConditionalBranch { instructionId = iid
                                , instructionType = it
                                , instructionBasicBlock = bb
                                , branchOperand1 = op1
                                , branchOperand2 = op2
                                , branchTestType = test
                                , branchTarget = dest
                                , branchFallthrough = fallthrough
                                }
      return (i, addInstruction i k0)
    12 -> do
      t <- getBB
      let i = UnconditionalBranch { instructionId = iid
                                  , instructionType = it
                                  , instructionBasicBlock = bb
                                  , branchTarget = t
                                  }
      return (i, addInstruction i k0)
    13 -> do
      sv <- getValue
      rawTs <- S.get
      ft <- getBB
      let i = Switch { instructionId = iid
                     , instructionType = it
                     , instructionBasicBlock = bb
                     , switchValue = sv
                     , switchTargets = map (second toBlock) rawTs
                     , switchFallthrough = ft
                     }
      return (i, addInstruction i k0)
    14 -> do
      op <- S.get
      op1 <- getValue
      op2 <- getValue
      let i = Compare { instructionId = iid
                      , instructionType = it
                      , instructionBasicBlock = bb
                      , compareOperation = op
                      , compareOperand1 = op1
                      , compareOperand2 = op2
                      }
      return (i, addInstruction i k0)
    15 -> do
      v <- getValue
      op <- S.get
      let i = UnaryOp { instructionId = iid
                      , instructionType = it
                      , instructionBasicBlock = bb
                      , unaryOperand = v
                      , unaryOperation = op
                      }
      return (i, addInstruction i k0)
    16 -> do
      op1 <- getValue
      op2 <- getValue
      op <- S.get
      let i = BinaryOp { instructionId = iid
                       , instructionType = it
                       , instructionBasicBlock = bb
                       , binaryOperand1 = op1
                       , binaryOperand2 = op2
                       , binaryOperation = op
                       }
      return (i, addInstruction i k0)
    17 -> do
      r <- getValue
      ix <- getValue
      let i = ArrayGet { instructionId = iid
                       , instructionType = it
                       , instructionBasicBlock = bb
                       , arrayReference = r
                       , arrayIndex = ix
                       }
      return (i, addInstruction i k0)
    18 -> do
      r <- getValue
      ix <- getValue
      v <- getValue
      let i = ArrayPut { instructionId = iid
                       , instructionType = it
                       , instructionBasicBlock = bb
                       , arrayReference = r
                       , arrayIndex = ix
                       , arrayPutValue = v
                       }
      return (i, addInstruction i k0)
    19 -> do
      f <- getF
      let i = StaticGet { instructionId = iid
                        , instructionType = it
                        , instructionBasicBlock = bb
                        , staticOpField = f
                        }
      return (i, addInstruction i k0)
    20 -> do
      f <- getF
      v <- getValue
      let i = StaticPut { instructionId = iid
                        , instructionType = it
                        , instructionBasicBlock = bb
                        , staticOpField = f
                        , staticOpPutValue = v
                        }
      return (i, addInstruction i k0)
    21 -> do
      r <- getValue
      f <- getF
      let i = InstanceGet { instructionId = iid
                          , instructionType = it
                          , instructionBasicBlock = bb
                          , instanceOpReference = r
                          , instanceOpField = f
                          }
      return (i, addInstruction i k0)
    22 -> do
      r <- getValue
      f <- getF
      v <- getValue
      let i = InstancePut { instructionId = iid
                          , instructionType = it
                          , instructionBasicBlock = bb
                          , instanceOpReference = r
                          , instanceOpField = f
                          , instanceOpPutValue = v
                          }
      return (i, addInstruction i k0)
    23 -> do
      k <- S.get
      mref <- getMRef
      args <- map toVal <$> S.get
      case NEL.nonEmpty args of
        Nothing -> error ("Encountered an empty argument list for InvokeVirtual at instruction " ++ show iid)
        Just args' -> do
          let i = InvokeVirtual { instructionId = iid
                                , instructionType = it
                                , instructionBasicBlock = bb
                                , invokeVirtualKind = k
                                , invokeVirtualMethod = mref
                                , invokeVirtualArguments = args'
                                }
          return (i, addInstruction i k0)
    24 -> do
      let methodErr m = error ("No method for id " ++ show m ++ " while decoding instruction " ++ show iid)
          toM m = fromMaybe (methodErr m) $ IM.lookup m (knotMethods fknot)
      k <- S.get
      mref <- getMRef
      m <- fmap toM <$> S.get
      args <- map toVal <$> S.get
      let i = InvokeDirect { instructionId = iid
                           , instructionType = it
                           , instructionBasicBlock = bb
                           , invokeDirectKind = k
                           , invokeDirectMethod = mref
                           , invokeDirectMethodDef = m
                           , invokeDirectArguments = args
                           }
      return (i, addInstruction i k0)
    25 -> do
      ivs <- map (\(b, v) -> (toBlock b, toVal v)) <$> S.get
      let i = Phi { instructionId = iid
                  , instructionType = it
                  , instructionBasicBlock = bb
                  , phiValues = ivs
                  }
      return (i, addInstruction i k0)
    _ -> error ("Unexpected instruction tag " ++ show tag)
  where
    tt = knotTypeTable k0

{-# INLINE addInstruction #-}
addInstruction :: Instruction -> Knot -> Knot
addInstruction i k = k { knotValues = IM.insert (instructionId i) (toValue i) (knotValues k) }

putMethodRef :: Map Type Int -> MethodRef -> S.Put
putMethodRef tt mref = do
  S.put (methodRefId mref)
  putType tt (methodRefClass mref)
  putType tt (methodRefReturnType mref)
  putList (putType tt) (methodRefParameterTypes mref)
  S.put (methodRefName mref)

getMethodRef :: Knot -> S.Get (MethodRef, Knot)
getMethodRef k0 = do
  rid <- S.get
  let tt = knotTypeTable k0
  ct <- getType tt
  rt <- getType tt
  pts <- getList (getType tt)
  name <- S.get
  let mref = MethodRef { methodRefId = rid
                       , methodRefClass = ct
                       , methodRefReturnType = rt
                       , methodRefParameterTypes = pts
                       , methodRefName = name
                       }
      k1 = k0 { knotMethodRefs = IM.insert rid mref (knotMethodRefs k0) }
  return (mref, k1)

putList :: (a -> S.PutM ()) -> [a] -> S.Put
putList p l = do
  S.putWord64le (fromIntegral (length l))
  mapM_ p l

getList :: S.Get a -> S.Get [a]
getList p = S.getWord64le >>= go []
  where
    go lst 0 = return (reverse lst)
    go lst !n = do
      elt <- p
      go (elt : lst) (n - 1)

getListAccum :: (k -> S.Get (a, k)) -> k -> S.Get ([a], k)
getListAccum p knot0 = S.getWord64le >>= go ([], knot0)
  where
    go (lst, !k) 0 = return (reverse lst, k)
    go (lst, !k) !n = do
      (elt, k') <- p k
      go (elt : lst, k') (n - 1)

