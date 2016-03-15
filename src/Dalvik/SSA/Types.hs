{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Dalvik.SSA.Types (
  DexFile(..),
  dexFileClass,
  dexFields,
  dexMethodRefs,
  dexConstants,
  dexTypes,
  dexClasses,
  dexClassAnnotation,
  dexMethodAnnotation,
  Type(..),
  Class(..),
  classStaticField,
  classInstanceField,
  classMethods,
  classVirtualMethods,
  classDirectMethods,
  classInstanceFields,
  classStaticFields,
  classInterfaces,
  Field(..),
  Method(..),
  MethodSignature,
  methodSignature,
  methodIsVirtual,
  methodParameters,
  methodBody,
  Parameter(..),
  BasicBlock(..),
  basicBlockSuccessors,
  basicBlockPredecessors,
  basicBlockInstructions,
  basicBlockTerminator,
  basicBlockSplitPhis,
  MethodRef(..),
  UniqueId,
  Value(..),
  IsValue(..),
  FromValue(..),
  CastException(..),
  Constant(..),
  constantId,
  constantType,
  Instruction(..),
  instructionOperands,
  InvokeDirectKind(..),
  InvokeVirtualKind(..),
  VisibleAnnotation(..),
  Annotation(..),
  AnnotationValue(..),
  DT.AnnotationVisibility(..),
  LL.CmpOp(..),
  LL.IfOp(..),
  LL.Binop(..),
  LL.Unop(..),
  LL.CType(..),
  module Dalvik.AccessFlags,
  module Dalvik.ClassName
  ) where

import GHC.Generics ( Generic )

import Control.DeepSeq
import qualified Control.Monad.Catch as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.Function ( on )
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe ( fromMaybe, maybeToList )
import qualified Data.Serialize as S
import qualified Data.Set as Set
import Data.Typeable ( Typeable )
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dalvik.AccessFlags
import Dalvik.ClassName
-- Low-level instructions
import qualified Dalvik.Instruction as LL
import qualified Dalvik.Types as DT

-- | A Dalvik Dex file represented in SSA form.
data DexFile =
  DexFile { _dexClasses :: !(Vector Class)
          , _dexConstants :: !(Vector Constant)
          , _dexTypes :: !(Vector Type)
          , dexIdSrc :: !Int
          , _dexClassesByType :: !(HashMap Type Class)
          , _dexClassesByName :: !(HashMap BS.ByteString Class)
            -- ^ An index by Class.className to the class. These are
            -- dotted fully-qualified names
          , _dexClassAnnotations :: !(HashMap Class [VisibleAnnotation])
          , _dexMethodAnnotations :: !(HashMap MethodAnnotationKey (MethodRef, [VisibleAnnotation]))
            -- ^ See Note [Annotations]
          } deriving (Typeable)

type MethodAnnotationKey = (Type, BS.ByteString, [Type])

-- | Look up the annotations on a 'Class'
dexClassAnnotation :: DexFile -> Class -> [VisibleAnnotation]
dexClassAnnotation df k = fromMaybe [] $ HM.lookup k (_dexClassAnnotations df)

-- | Look up the annotations on a 'Method'
dexMethodAnnotation :: DexFile -> Method -> [VisibleAnnotation]
dexMethodAnnotation df m = maybe [] snd $ HM.lookup key (_dexMethodAnnotations df)
  where
    (ptypes, _) = methodSignature m
    key = (classType (methodClass m), methodName m, ptypes)

dexFileClass :: DexFile -> Type -> Maybe Class
dexFileClass df t = HM.lookup t (_dexClassesByType df)

dexClasses :: DexFile -> [Class]
dexClasses = V.toList . _dexClasses

dexConstants :: DexFile -> [Constant]
dexConstants = V.toList . _dexConstants

dexTypes :: DexFile -> [Type]
dexTypes = V.toList . _dexTypes

data Value = InstructionV Instruction
           | ConstantV Constant
           | ParameterV Parameter

instance Eq Value where
  (==) = (==) `on` valueId

instance Ord Value where
  compare = compare `on` valueId

instance Hashable Value where
  hashWithSalt s = hashWithSalt s . valueId

-- | A common interface to the three 'Value'-like types.
class IsValue a where
  valueType :: a -> Type
  valueId :: a -> UniqueId
  toValue :: a -> Value

instance IsValue Value where
  valueId (InstructionV i) = instructionId i
  valueId (ConstantV c) = constantId c
  valueId (ParameterV p) = parameterId p

  valueType (InstructionV i) = instructionType i
  valueType (ConstantV c) = constantType c
  valueType (ParameterV p) = parameterType p

  toValue = id

-- | Convenient and safe casting from 'Value' to a concrete type
-- (either 'Constant', 'Instruction', or 'Parameter').
class FromValue a where
  fromValue :: (E.MonadThrow m) => Value -> m a

data CastException = CastException String
                   deriving (Eq, Ord, Show, Typeable)

instance E.Exception CastException

-- | The 'VisibleAnnotation' is a wrapper around a Java 'Annotation'
-- that records the visibility of the annotation.  Some annotations
-- can be discarded at build time, while others are available at run
-- time to the application.
data VisibleAnnotation = VisibleAnnotation { annotationVisibility :: DT.AnnotationVisibility
                                           , annotationValue :: Annotation
                                           }

-- | An annotation has a type (which is always a class type) and
-- compile-time constant arguments.  The argument language available
-- is separate from the constants used in the rest of the IR.
data Annotation = Annotation { annotationType :: Type
                               -- ^ This must be a class (i.e.,
                               -- reference type); should this just be
                               -- a ClassName?
                             , annotationArguments :: [(BS.ByteString, AnnotationValue)]
                             }
                deriving (Eq, Ord, Typeable)

data AnnotationValue = AVInt !Integer
                     | AVChar !Char
                     | AVFloat !Float
                     | AVDouble !Double
                     | AVString BS.ByteString
                     | AVType Type
                     | AVField Field
                     | AVMethod MethodRef
                     | AVEnum Field
                     | AVArray [AnnotationValue]
                     | AVAnnotation Annotation
                     | AVNull
                     | AVBool !Bool
                     deriving (Eq, Ord, Typeable)


-- FIXME: For now, all numeric constants are integer types because we
-- can't tell (without a type inference pass) what type a constant
-- really is.  Dalvik is untyped.
data Constant = ConstantInt {-# UNPACK #-} !UniqueId {-# UNPACK #-} !Int64
              | ConstantString {-# UNPACK #-} !UniqueId BS.ByteString
              | ConstantClass {-# UNPACK #-} !UniqueId Type
                deriving (Typeable)

instance Eq Constant where
  (==) = (==) `on` constantId

instance Ord Constant where
  compare = compare `on` constantId

instance Hashable Constant where
  hashWithSalt s = hashWithSalt s . constantId

instance NFData Constant where
  rnf c = c `seq` ()

constantId :: Constant -> Int
constantId (ConstantInt i _) = i
constantId (ConstantString i _) = i
constantId (ConstantClass i _) = i

constantType :: Constant -> Type
constantType (ConstantString _ _) =
  ReferenceType (qualifiedClassName ["java", "lang"] "String")
constantType (ConstantClass _ _) =
  ReferenceType (qualifiedClassName ["java", "lang"] "Class")
constantType _ = UnknownType

instance IsValue Constant where
  valueId = constantId
  valueType = constantType
  toValue = ConstantV

instance FromValue Constant where
  fromValue (ConstantV c) = return c
  fromValue _ = E.throwM $ CastException "Not a Constant"

data Type = VoidType
          | ByteType
          | ShortType
          | IntType
          | LongType
          | FloatType
          | DoubleType
          | CharType
          | BooleanType
          | ArrayType Type
          | ReferenceType ClassName
          | UnknownType
            -- ^ We use this in cases where we can't deduce a type
            -- during the SSA translation
          deriving (Eq, Ord, Read, Show, Generic)

instance S.Serialize Type

instance Hashable Type where
  hashWithSalt s VoidType = hashWithSalt s (1 :: Int)
  hashWithSalt s ByteType = hashWithSalt s (2 :: Int)
  hashWithSalt s ShortType = hashWithSalt s (3 :: Int)
  hashWithSalt s IntType = hashWithSalt s (4 :: Int)
  hashWithSalt s LongType = hashWithSalt s (5 :: Int)
  hashWithSalt s FloatType = hashWithSalt s (6 :: Int)
  hashWithSalt s DoubleType = hashWithSalt s (7 :: Int)
  hashWithSalt s CharType = hashWithSalt s (8 :: Int)
  hashWithSalt s BooleanType = hashWithSalt s (9 :: Int)
  hashWithSalt s (ArrayType t) = s `hashWithSalt` (10 :: Int) `hashWithSalt` t
  hashWithSalt s (ReferenceType cname) = s `hashWithSalt` (11 :: Int) `hashWithSalt` cname
  hashWithSalt s UnknownType = hashWithSalt s (12 :: Int)

-- | A basic block containing 'Instruction's.  We maintain a count of
-- the phi nodes in the block so that we can efficiently slice out
-- either instructions or phi nodes for separate processing.
--
-- The 'basicBlockNumber' is the local identifier (within a method) of
-- a 'BasicBlock'.  The unique ID is unique among all 'BasicBlock's in
-- the 'DexFile'.
data BasicBlock = BasicBlock { basicBlockId :: {-# UNPACK #-} !UniqueId
                             , basicBlockNumber :: {-# UNPACK #-} !Int
                             , basicBlockPhiCount :: {-# UNPACK #-} !Int
                             , _basicBlockInstructions :: Vector Instruction
                             , _basicBlockSuccessors :: Vector BasicBlock
                             , _basicBlockPredecessors :: Vector BasicBlock
                             , basicBlockMethod :: Method
                             }

-- | The instructions in the 'BasicBlock'
basicBlockInstructions :: BasicBlock -> [Instruction]
basicBlockInstructions = V.toList . _basicBlockInstructions

basicBlockSuccessors :: BasicBlock -> [BasicBlock]
basicBlockSuccessors = V.toList . _basicBlockSuccessors

basicBlockPredecessors :: BasicBlock -> [BasicBlock]
basicBlockPredecessors = V.toList . _basicBlockPredecessors

-- | The last non-phi instruction in the block.
--
-- There is always a final non-phi instruction because we insert
-- explicit control flow transfers as necessary.  However, the
-- returned instruction may not be a typical terminator instruction in
-- some cases.  See Note [Non-Empty Blocks] in Dalvik.SSA for details.
basicBlockTerminator :: BasicBlock -> Instruction
basicBlockTerminator bb = insns V.! (len - 1)
  where
    insns = _basicBlockInstructions bb
    len = V.length insns

-- | Split the instructions in the 'BasicBlock' into phi nodes and the
-- rest.
basicBlockSplitPhis :: BasicBlock -> ([Instruction], [Instruction])
basicBlockSplitPhis bb = (V.toList v1, V.toList v2)
  where
    (v1, v2) = V.splitAt (basicBlockPhiCount bb) (_basicBlockInstructions bb)

instance Eq BasicBlock where
  (==) = (==) `on` basicBlockId

instance Ord BasicBlock where
  compare = compare `on` basicBlockId

instance Hashable BasicBlock where
  hashWithSalt s = hashWithSalt s . basicBlockId

type UniqueId = Int

data Instruction = Return { instructionId :: {-# UNPACK #-} !UniqueId
                          , instructionType :: Type
                          , instructionBasicBlock :: BasicBlock
                          , returnValue :: Maybe Value
                          }
                 | MoveException { instructionId :: {-# UNPACK #-} !UniqueId
                                 , instructionType :: Type
                                 , instructionBasicBlock :: BasicBlock
                                 }
                 | MonitorEnter { instructionId :: {-# UNPACK #-} !UniqueId
                                , instructionType :: Type
                                , instructionBasicBlock :: BasicBlock
                                , monitorReference :: Value
                                }
                 | MonitorExit { instructionId :: {-# UNPACK #-} !UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , monitorReference :: Value
                               }
                 | CheckCast { instructionId :: {-# UNPACK #-} !UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , castReference :: Value
                             , castType :: Type
                             }
                 | InstanceOf { instructionId :: {-# UNPACK #-} !UniqueId
                              , instructionType :: Type
                              , instructionBasicBlock :: BasicBlock
                              , instanceOfReference :: Value
                              , instanceOfType :: Type
                              }
                 | ArrayLength { instructionId :: {-# UNPACK #-} !UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , arrayReference :: Value
                               }
                 | NewInstance { instructionId :: {-# UNPACK #-} !UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               }
                 | NewArray { instructionId :: {-# UNPACK #-} !UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , newArrayLength :: Value
                            , newArrayContents :: Maybe [Value]
                            }
                 | FillArray { instructionId :: {-# UNPACK #-} !UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , fillArrayReference :: Value
                             , fillArrayContents :: [Int64]
                             }
                 | Throw { instructionId :: {-# UNPACK #-} !UniqueId
                         , instructionType :: Type
                         , instructionBasicBlock :: BasicBlock
                         , throwReference :: Value
                         }
                 | ConditionalBranch { instructionId :: {-# UNPACK #-} !UniqueId
                                     , branchTestType :: !LL.IfOp
                                     , instructionType :: Type
                                     , instructionBasicBlock :: BasicBlock
                                     , branchOperand1 :: Value
                                     , branchOperand2 :: Value
                                     , branchTarget :: BasicBlock
                                     , branchFallthrough :: BasicBlock
                                     }
                 | UnconditionalBranch { instructionId :: {-# UNPACK #-} !UniqueId
                                       , instructionType :: Type
                                       , instructionBasicBlock :: BasicBlock
                                       , branchTarget :: BasicBlock
                                       }
                 | Switch { instructionId :: {-# UNPACK #-} !UniqueId
                          , instructionType :: Type
                          , instructionBasicBlock :: BasicBlock
                          , switchValue :: Value
                          , switchTargets :: [(Int64, BasicBlock)]
                          , switchFallthrough :: BasicBlock
                          }
                 | Compare { instructionId :: {-# UNPACK #-} !UniqueId
                           , compareOperation :: !LL.CmpOp
                           , instructionType :: Type
                           , instructionBasicBlock :: BasicBlock
                           , compareOperand1 :: Value
                           , compareOperand2 :: Value
                           }
                 | UnaryOp { instructionId :: {-# UNPACK #-} !UniqueId
                           , unaryOperation :: !LL.Unop
                           , instructionType :: Type
                           , instructionBasicBlock :: BasicBlock
                           , unaryOperand :: Value
                           }
                 | BinaryOp { instructionId :: {-# UNPACK #-} !UniqueId
                            , binaryOperation :: !LL.Binop
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , binaryOperand1 :: Value
                            , binaryOperand2 :: Value
                            }
                 | ArrayGet { instructionId :: {-# UNPACK #-} !UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            }
                 | ArrayPut { instructionId :: {-# UNPACK #-} !UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            , arrayPutValue :: Value
                            }
                 | StaticGet { instructionId :: {-# UNPACK #-} !UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , staticOpField :: Field
                             }
                 | StaticPut { instructionId :: {-# UNPACK #-} !UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , staticOpField :: Field
                             , staticOpPutValue :: Value
                             }
                 | InstanceGet { instructionId :: {-# UNPACK #-} !UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               }
                 | InstancePut { instructionId :: {-# UNPACK #-} !UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               , instanceOpPutValue :: Value
                            }
                 | InvokeVirtual { instructionId :: {-# UNPACK #-} !UniqueId
                                 , invokeVirtualKind :: !InvokeVirtualKind
                                 , instructionType :: Type
                                 , instructionBasicBlock :: BasicBlock
                                 , invokeVirtualMethod :: MethodRef
                                 , invokeVirtualArguments :: NonEmpty Value
                                 }
                 | InvokeDirect { instructionId :: {-# UNPACK #-} !UniqueId
                                , invokeDirectKind :: !InvokeDirectKind
                                , instructionType :: Type
                                , instructionBasicBlock :: BasicBlock
                                , invokeDirectMethod :: MethodRef
                                , invokeDirectMethodDef :: Maybe Method
                                , invokeDirectArguments :: [Value]
                                }
                   -- ^ We have to refer to the invoked method in this
                   -- case by a Ref because it might not have a
                   -- definition in this dex file.  However, we do
                   -- include the definition of the invoked method if
                   -- it is available.
                 | Phi { instructionId :: {-# UNPACK #-} !UniqueId
                       , instructionType :: Type
                       , instructionBasicBlock :: BasicBlock
                       , phiValues :: [(BasicBlock, Value)]
                       }

instance Eq Instruction where
  (==) = (==) `on` instructionId

instance Ord Instruction where
  compare = compare `on` instructionId

instance Hashable Instruction where
  hashWithSalt s = hashWithSalt s . instructionId

instance IsValue Instruction where
  valueId = instructionId
  valueType = instructionType
  toValue = InstructionV

instance FromValue Instruction where
  fromValue (InstructionV i) = return i
  fromValue _ = E.throwM $ CastException "Not an Instruction"

data Parameter = Parameter { parameterId :: {-# UNPACK #-} !UniqueId
                           , parameterIndex :: {-# UNPACK #-} !Int
                           , parameterType :: Type
                           , parameterName :: BS.ByteString
                           , parameterMethod :: Method
                           }

instance Eq Parameter where
  (==) = (==) `on` parameterId

instance Ord Parameter where
  compare = compare `on` parameterId

instance Hashable Parameter where
  hashWithSalt s = hashWithSalt s . parameterId

instance IsValue Parameter where
  valueId = parameterId
  valueType = parameterType
  toValue = ParameterV

instance FromValue Parameter where
  fromValue (ParameterV p) = return p
  fromValue _ = E.throwM $ CastException "Not a Parameter"

data Method = Method { methodId :: {-# UNPACK #-} !UniqueId
                     , methodAccessFlags :: {-# UNPACK #-} !AccessFlags
                     , methodName :: BS.ByteString
                     , methodReturnType :: Type
                     , _methodParameters :: Vector Parameter
                     , _methodBody :: Maybe (Vector BasicBlock)
                     , methodClass :: Class
                     }

methodIsVirtual :: Method -> Bool
methodIsVirtual = not . hasAccessFlag ACC_STATIC . methodAccessFlags

methodParameters :: Method -> [Parameter]
methodParameters = V.toList . _methodParameters

methodBody :: Method -> Maybe [BasicBlock]
methodBody = fmap V.toList . _methodBody

type MethodSignature = ([Type], Type)

methodSignature :: Method -> MethodSignature
methodSignature m = (map parameterType ps, methodReturnType m)
  where
    ps = case methodIsVirtual m of
      False -> methodParameters m
      True -> case methodParameters m of
        [] -> error "methodSignature: No parameters in a virtual function"
        _:rest -> rest

instance NFData Method where
  rnf m = m `seq` ()

instance Eq Method where
  (==) = (==) `on` methodId

instance Ord Method where
  compare = compare `on` methodId

instance Hashable Method where
  hashWithSalt s = hashWithSalt s . methodId

data Class = Class { classId :: {-# UNPACK #-} !UniqueId
                   , classAccessFlags :: {-# UNPACK #-} !AccessFlags
                   , classType :: Type
                   , className :: BS.ByteString
                   , classSourceName :: BS.ByteString
                   , classParent :: Maybe Type
                   , classParentReference :: Maybe Class
                   , _classInterfaces :: Vector Type
                   , _classDirectMethods :: Vector Method
                   , _classVirtualMethods :: Vector Method
                   , _classStaticFields :: Vector (AccessFlags, Field)
                   , _classInstanceFields :: Vector (AccessFlags, Field)
                   , _classStaticFieldMap :: HashMap BS.ByteString Field
                   , _classInstanceFieldMap :: HashMap BS.ByteString Field
                   , _classMethodMap :: HashMap (BS.ByteString, MethodSignature) Method
                     -- ^ A map of method name + signature to methods
                   }

classInterfaces :: Class -> [Type]
classInterfaces = V.toList . _classInterfaces

classDirectMethods :: Class -> [Method]
classDirectMethods = V.toList . _classDirectMethods

classVirtualMethods :: Class -> [Method]
classVirtualMethods = V.toList . _classVirtualMethods

classMethods :: Class -> [Method]
classMethods klass = classDirectMethods klass ++ classVirtualMethods klass

classStaticFields :: Class -> [(AccessFlags, Field)]
classStaticFields = V.toList . _classStaticFields

classInstanceFields :: Class -> [(AccessFlags, Field)]
classInstanceFields = V.toList . _classInstanceFields

classStaticField :: Class -> BS.ByteString -> Maybe Field
classStaticField k s = HM.lookup s (_classStaticFieldMap k)

classInstanceField :: Class -> BS.ByteString -> Maybe Field
classInstanceField k s = HM.lookup s (_classInstanceFieldMap k)

instance Eq Class where
  (==) = (==) `on` classId

instance Ord Class where
  compare = compare `on` classId

instance Hashable Class where
  hashWithSalt s = hashWithSalt s . classId

data Field = Field { fieldId :: {-# UNPACK #-} !UniqueId
                   , fieldName :: BS.ByteString
                   , fieldType :: Type
                   , fieldClass :: Type
                   }

instance Eq Field where
  (==) = (==) `on` fieldId

instance Ord Field where
  compare = compare `on` fieldId

instance Hashable Field where
  hashWithSalt s = hashWithSalt s . fieldId

data MethodRef = MethodRef { methodRefId :: {-# UNPACK #-} !UniqueId
                           , methodRefClass :: Type
                           , methodRefReturnType :: Type
                           , methodRefParameterTypes :: [Type]
                           , methodRefName :: BS.ByteString
                           }

instance Eq MethodRef where
  (==) = (==) `on` methodRefId

instance Ord MethodRef where
  compare = compare `on` methodRefId

instance Hashable MethodRef where
  hashWithSalt s = hashWithSalt s . methodRefId

data InvokeDirectKind = MethodInvokeStatic
                      | MethodInvokeDirect
                      deriving (Eq, Ord, Show, Generic)

instance S.Serialize InvokeDirectKind

instance Hashable InvokeDirectKind where
  hashWithSalt s MethodInvokeStatic = hashWithSalt s (1 :: Int)
  hashWithSalt s MethodInvokeDirect = hashWithSalt s (2 :: Int)

data InvokeVirtualKind = MethodInvokeInterface
                       | MethodInvokeSuper
                       | MethodInvokeVirtual
                       deriving (Eq, Ord, Show, Generic)

instance S.Serialize InvokeVirtualKind

instance Hashable InvokeVirtualKind where
  hashWithSalt s MethodInvokeInterface = hashWithSalt s (1 :: Int)
  hashWithSalt s MethodInvokeSuper = hashWithSalt s (2 :: Int)
  hashWithSalt s MethodInvokeVirtual = hashWithSalt s (3 :: Int)


-- | Returns a view of the operands of an instruction
--
-- Only 'Value' operands are included.  Constant Ints are not, nor are
-- Types.
instructionOperands :: Instruction -> [Value]
instructionOperands i =
  case i of
    Return { returnValue = rv } -> maybe [] (:[]) rv
    MoveException {} -> []
    MonitorEnter { monitorReference = v } -> [v]
    MonitorExit { monitorReference = v } -> [v]
    CheckCast { castReference = r } -> [r]
    InstanceOf { instanceOfReference = r } -> [r]
    ArrayLength { arrayReference = r } -> [r]
    NewInstance {} -> []
    NewArray { newArrayLength = l
             , newArrayContents = vs
             } -> l : fromMaybe [] vs
    FillArray { fillArrayReference = r } -> [r]
    Throw { throwReference = r } -> [r]
    ConditionalBranch { branchOperand1 = o1
                      , branchOperand2 = o2
                      } -> [o1, o2]
    UnconditionalBranch {} -> []
    Switch { switchValue = v } -> [v]
    Compare { compareOperand1 = op1
            , compareOperand2 = op2
            } -> [op1, op2]
    UnaryOp { unaryOperand = op } -> [op]
    BinaryOp { binaryOperand1 = op1
             , binaryOperand2 = op2
             } -> [op1, op2]
    ArrayGet { arrayReference = r
             , arrayIndex = ix
             } -> [r, ix]
    ArrayPut { arrayReference = r
             , arrayIndex = ix
             , arrayPutValue = v
             } -> [r, ix, v]
    StaticGet {} -> []
    StaticPut { staticOpPutValue = v } -> [v]
    InstanceGet { instanceOpReference = r } -> [r]
    InstancePut { instanceOpReference = r
                , instanceOpPutValue = v
                } -> [r, v]
    InvokeVirtual { invokeVirtualArguments = args } -> F.toList args
    InvokeDirect { invokeDirectArguments = args } -> args
    Phi { phiValues = ivs } -> map snd ivs

-- | All of the fields referenced in a dex file.
--
-- This is a superset of the fields contained by all classes.  In the
-- case where an instruction references a field not defined in the dex
-- file, 'dexFields' will return it while just inspecting the fields
-- of all available classes will not find it.
dexFields :: DexFile -> [Field]
dexFields df = Set.toList $ Set.fromList $ concat [ifields, cfields, afields]
  where
    cfields = [ f
              | klass <- dexClasses df
              , (_, f) <- classStaticFields klass ++ classInstanceFields klass
              ]
    ifields = [ f
              | k <- dexClasses df
              , m <- classDirectMethods k ++ classVirtualMethods k
              , body <- maybeToList (methodBody m)
              , block <- body
              , i <- basicBlockInstructions block
              , f <- referencedField i
              ]
    afields = [ f
              | va <- allAnnotations df
              , (_, val) <- annotationArguments (annotationValue va)
              , f <- annotationValueFieldRefs val
              ]
    referencedField i =
      case i of
        StaticGet { staticOpField = f } -> [f]
        StaticPut { staticOpField = f } -> [f]
        InstanceGet { instanceOpField = f } -> [f]
        InstancePut { instanceOpField = f } -> [f]
        _ -> []

dexMethodRefs :: DexFile -> [MethodRef]
dexMethodRefs df =
  Set.toList $ Set.fromList $ concat [ irefs, arefs, amrefs ]
  where
    irefs = [ mref
            | k <- dexClasses df
            , m <- classDirectMethods k ++ classVirtualMethods k
            , body <- maybeToList (methodBody m)
            , block <- body
            , i <- basicBlockInstructions block
            , mref <- maybeMethodRef i
            ]
    arefs = [ mref
            | va <- allAnnotations df
            , (_, val) <- annotationArguments (annotationValue va)
            , mref <- annotationValueMethodRefs val
            ]
    amrefs = [ mref
             | (mref, _) <- HM.elems (_dexMethodAnnotations df)
             ]
    maybeMethodRef i =
      case i of
        InvokeVirtual { invokeVirtualMethod = mref } -> [mref]
        InvokeDirect { invokeDirectMethod = mref } -> [mref]
        _ -> []

allAnnotations :: DexFile -> [VisibleAnnotation]
allAnnotations df = concat $ map snd (HM.toList (_dexClassAnnotations df)) ++ map (snd . snd) (HM.toList (_dexMethodAnnotations df))

annotationValueFieldRefs :: AnnotationValue -> [Field]
annotationValueFieldRefs = go []
  where
    go acc v =
      case v of
        AVField f -> f : acc
        AVEnum f -> f : acc
        AVArray vs -> F.foldl' go acc vs
        AVAnnotation a -> F.foldl' go acc [ val | (_, val) <- annotationArguments a ]
        _ -> acc

annotationValueMethodRefs :: AnnotationValue -> [MethodRef]
annotationValueMethodRefs = go []
  where
    go acc v =
      case v of
        AVMethod m -> m : acc
        AVArray vs -> F.foldl' go acc vs
        AVAnnotation a -> F.foldl' go acc [ val | (_, val) <- annotationArguments a ]
        _ -> acc

{- Note [Annotations]

All of classes, methods, fields, and individual parameters can have
annotations.

Instead of putting annotations directly on those objects in the IR, we
store them in side tables (held by the top-level DexFile).  This is
mostly a matter of implementation convenience, excused by the fact
that annotations are accessed only infrequently.

-}
