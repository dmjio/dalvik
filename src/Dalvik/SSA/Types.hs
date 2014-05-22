{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Dalvik.SSA.Types (
  DexFile(..),
  dexFileClass,
  Type(..),
  Class(..),
  classStaticField,
  classInstanceField,
  classMethods,
  Field(..),
  Method(..),
  methodSignature,
  methodIsVirtual,
  Parameter(..),
  BasicBlock(..),
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
  LL.CmpOp(..),
  LL.IfOp(..),
  LL.Binop(..),
  LL.Unop(..),
  LL.CType(..),
  module Dalvik.AccessFlags,
  module Dalvik.ClassName
  ) where

import GHC.Generics ( Generic )

import qualified Control.Monad.Catch as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.Function ( on )
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Int ( Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( Typeable )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Text.Show.Pretty as PP

import Dalvik.AccessFlags
import Dalvik.ClassName
-- Low-level instructions
import qualified Dalvik.Instruction as LL

-- | A Dalvik Dex file represented in SSA form.
data DexFile =
  DexFile { dexClasses :: [Class]
          , dexConstants :: [Constant]
          , dexTypes :: [Type]
          , _dexClassesByType :: HashMap Type Class
          }

dexFileClass :: DexFile -> Type -> Maybe Class
dexFileClass df t = HM.lookup t (_dexClassesByType df)

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


-- FIXME: For now, all numeric constants are integer types because we
-- can't tell (without a type inference pass) what type a constant
-- really is.  Dalvik is untyped.
data Constant = ConstantInt !UniqueId !Int64
              | ConstantString !UniqueId BS.ByteString
              | ConstantClass !UniqueId Type

instance Eq Constant where
  (==) = (==) `on` constantId

instance Ord Constant where
  compare = compare `on` constantId

instance Hashable Constant where
  hashWithSalt s = hashWithSalt s . constantId

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

instance PP.PrettyVal Type

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
data BasicBlock = BasicBlock { basicBlockId :: UniqueId
                             , basicBlockNumber :: Int
                             , _basicBlockInstructions :: Vector Instruction
                             , basicBlockPhiCount :: Int
                             , basicBlockSuccessors :: [BasicBlock]
                             , basicBlockPredecessors :: [BasicBlock]
                             , basicBlockMethod :: Method
                             }

-- | The instructions in the 'BasicBlock'
basicBlockInstructions :: BasicBlock -> [Instruction]
basicBlockInstructions = V.toList . _basicBlockInstructions

-- | The last non-phi instruction in the block.  There may not be one
-- if the block is empty.
--
-- Note: we could have this always succeed by adding explicit jumps at
-- the end of empty blocks.
basicBlockTerminator :: BasicBlock -> Maybe Instruction
basicBlockTerminator bb
  | basicBlockPhiCount bb >= len = Nothing
  | otherwise = Just $ insns V.! (len - 1)
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

data Instruction = Return { instructionId :: UniqueId
                          , instructionType :: Type
                          , instructionBasicBlock :: BasicBlock
                          , returnValue :: Maybe Value
                          }
                 | MoveException { instructionId :: UniqueId
                                 , instructionType :: Type
                                 , instructionBasicBlock :: BasicBlock
                                 }
                 | MonitorEnter { instructionId :: UniqueId
                                , instructionType :: Type
                                , instructionBasicBlock :: BasicBlock
                                , monitorReference :: Value
                                }
                 | MonitorExit { instructionId :: UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , monitorReference :: Value
                               }
                 | CheckCast { instructionId :: UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , castReference :: Value
                             , castType :: Type
                             }
                 | InstanceOf { instructionId :: UniqueId
                              , instructionType :: Type
                              , instructionBasicBlock :: BasicBlock
                              , instanceOfReference :: Value
                              , instanceOfType :: Type
                              }
                 | ArrayLength { instructionId :: UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , arrayReference :: Value
                               }
                 | NewInstance { instructionId :: UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               }
                 | NewArray { instructionId :: UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , newArrayLength :: Value
                            , newArrayContents :: Maybe [Value]
                            }
                 | FillArray { instructionId :: UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , fillArrayReference :: Value
                             , fillArrayContents :: [Int64]
                             }
                 | Throw { instructionId :: UniqueId
                         , instructionType :: Type
                         , instructionBasicBlock :: BasicBlock
                         , throwReference :: Value
                         }
                 | ConditionalBranch { instructionId :: UniqueId
                                     , instructionType :: Type
                                     , instructionBasicBlock :: BasicBlock
                                     , branchOperand1 :: Value
                                     , branchOperand2 :: Value
                                     , branchTestType :: LL.IfOp
                                     , branchTarget :: BasicBlock
                                     , branchFallthrough :: BasicBlock
                                     }
                 | UnconditionalBranch { instructionId :: UniqueId
                                       , instructionType :: Type
                                       , instructionBasicBlock :: BasicBlock
                                       , branchTarget :: BasicBlock
                                       }
                 | Switch { instructionId :: UniqueId
                          , instructionType :: Type
                          , instructionBasicBlock :: BasicBlock
                          , switchValue :: Value
                          , switchTargets :: [(Int64, BasicBlock)]
                          , switchFallthrough :: BasicBlock
                          }
                 | Compare { instructionId :: UniqueId
                           , instructionType :: Type
                           , instructionBasicBlock :: BasicBlock
                           , compareOperation :: LL.CmpOp
                           , compareOperand1 :: Value
                           , compareOperand2 :: Value
                           }
                 | UnaryOp { instructionId :: UniqueId
                           , instructionType :: Type
                           , instructionBasicBlock :: BasicBlock
                           , unaryOperand :: Value
                           , unaryOperation :: LL.Unop
                           }
                 | BinaryOp { instructionId :: UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , binaryOperand1 :: Value
                            , binaryOperand2 :: Value
                            , binaryOperation :: LL.Binop
                            }
                 | ArrayGet { instructionId :: UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            }
                 | ArrayPut { instructionId :: UniqueId
                            , instructionType :: Type
                            , instructionBasicBlock :: BasicBlock
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            , arrayPutValue :: Value
                            }
                 | StaticGet { instructionId :: UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , staticOpField :: Field
                             }
                 | StaticPut { instructionId :: UniqueId
                             , instructionType :: Type
                             , instructionBasicBlock :: BasicBlock
                             , staticOpField :: Field
                             , staticOpPutValue :: Value
                             }
                 | InstanceGet { instructionId :: UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               }
                 | InstancePut { instructionId :: UniqueId
                               , instructionType :: Type
                               , instructionBasicBlock :: BasicBlock
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               , instanceOpPutValue :: Value
                            }
                 | InvokeVirtual { instructionId :: UniqueId
                                 , instructionType :: Type
                                 , instructionBasicBlock :: BasicBlock
                                 , invokeVirtualKind :: InvokeVirtualKind
                                 , invokeVirtualMethod :: MethodRef
                                 , invokeVirtualArguments :: NonEmpty Value
                                 }
                 | InvokeDirect { instructionId :: UniqueId
                                , instructionType :: Type
                                , instructionBasicBlock :: BasicBlock
                                , invokeDirectKind :: InvokeDirectKind
                                , invokeDirectMethod :: MethodRef
                                , invokeDirectMethodDef :: Maybe Method
                                , invokeDirectArguments :: [Value]
                                }
                   -- ^ We have to refer to the invoked method in this
                   -- case by a Ref because it might not have a
                   -- definition in this dex file.  However, we do
                   -- include the definition of the invoked method if
                   -- it is available.
                 | Phi { instructionId :: UniqueId
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

data Parameter = Parameter { parameterId :: UniqueId
                           , parameterType :: Type
                           , parameterName :: BS.ByteString
                           , parameterIndex :: Int
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

data Method = Method { methodId :: UniqueId
                     , methodName :: BS.ByteString
                     , methodReturnType :: Type
                     , methodAccessFlags :: AccessFlags
                     , methodParameters :: [Parameter]
                     , methodBody :: Maybe [BasicBlock]
                     , methodClass :: Class
                     }

methodIsVirtual :: Method -> Bool
methodIsVirtual = not . hasAccessFlag ACC_STATIC . methodAccessFlags

methodSignature :: Method -> ([Type], Type)
methodSignature m = (map parameterType ps, methodReturnType m)
  where
    ps = case methodIsVirtual m of
      False -> methodParameters m
      True -> case methodParameters m of
        [] -> error "methodSignature: No parameters in a virtual function"
        _:rest -> rest

instance Eq Method where
  (==) = (==) `on` methodId

instance Ord Method where
  compare = compare `on` methodId

instance Hashable Method where
  hashWithSalt s = hashWithSalt s . methodId

data Class = Class { classId :: UniqueId
                   , classType :: Type
                   , className :: BS.ByteString
                   , classSourceName :: BS.ByteString
                   , classAccessFlags :: AccessFlags
                   , classParent :: Maybe Type
                   , classParentReference :: Maybe Class
                   , classInterfaces :: [Type]
                   , classDirectMethods :: [Method]
                   , classVirtualMethods :: [Method]
                   , classStaticFields :: [(AccessFlags, Field)]
                   , classInstanceFields :: [(AccessFlags, Field)]
                   , _classStaticFieldMap :: HashMap BS.ByteString Field
                   , _classInstanceFieldMap :: HashMap BS.ByteString Field
                   }

classStaticField :: Class -> BS.ByteString -> Maybe Field
classStaticField k s = HM.lookup s (_classStaticFieldMap k)

classInstanceField :: Class -> BS.ByteString -> Maybe Field
classInstanceField k s = HM.lookup s (_classInstanceFieldMap k)

classMethods :: Class -> [Method]
classMethods klass = classDirectMethods klass ++ classVirtualMethods klass

instance Eq Class where
  (==) = (==) `on` classId

instance Ord Class where
  compare = compare `on` classId

instance Hashable Class where
  hashWithSalt s = hashWithSalt s . classId

data Field = Field { fieldId :: UniqueId
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

data MethodRef = MethodRef { methodRefId :: UniqueId
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
                      deriving (Eq, Ord, Show)

instance Hashable InvokeDirectKind where
  hashWithSalt s MethodInvokeStatic = hashWithSalt s (1 :: Int)
  hashWithSalt s MethodInvokeDirect = hashWithSalt s (2 :: Int)

data InvokeVirtualKind = MethodInvokeInterface
                       | MethodInvokeSuper
                       | MethodInvokeVirtual
                       deriving (Eq, Ord, Show)

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
