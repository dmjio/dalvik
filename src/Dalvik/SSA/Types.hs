{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}
module Dalvik.SSA.Types (
  DexFile(..),
  Type(..),
  Class(..),
  Field(..),
  Method(..),
  Parameter(..),
  BasicBlock(..),
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
  InvokeDirectKind(..),
  InvokeVirtualKind(..),
  LL.CmpOp(..),
  LL.IfOp(..),
  LL.Binop(..),
  LL.Unop(..),
  LL.CType(..),
  module Dalvik.AccessFlags
  ) where

import Control.Exception ( Exception )
import Control.Failure
import Data.Function ( on )
import Data.Hashable
import Data.Int ( Int64 )
import Data.Typeable ( Typeable )
import Data.Vector ( Vector )

import Dalvik.AccessFlags
import Dalvik.ClassHierarchy
-- Low-level instructions
import qualified Dalvik.Instruction as LL

-- | A Dalvik Dex file represented in SSA form.
data DexFile =
  DexFile { dexIdentifier :: String
          , dexClasses :: [Class]
          }

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

instance IsValue Value where
  valueId (InstructionV i) = instructionId i
  valueId (ConstantV c) = constantId c
  valueId (ParameterV p) = parameterId p

  valueType (InstructionV i) = instructionType i
  valueType (ConstantV c) = constantType c
  valueType (ParameterV p) = parameterType p

-- | Convenient and safe casting from 'Value' to a concrete type
-- (either 'Constant', 'Instruction', or 'Parameter').
class FromValue a where
  fromValue :: (Failure CastException f) => Value -> f a

data CastException = CastException String
                   deriving (Eq, Ord, Show, Typeable)

instance Exception CastException


-- FIXME: For now, all numeric constants are integer types because we
-- can't tell (without a type inference pass) what type a constant
-- really is.  Dalvik is untyped.
data Constant = ConstantInt !UniqueId !Int64
              | ConstantString !UniqueId String
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
constantType _ = UnknownType

instance IsValue Constant where
  valueId = constantId
  valueType = constantType

instance FromValue Constant where
  fromValue (ConstantV c) = return c
  fromValue _ = failure $ CastException "Not a Constant"

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
          deriving (Eq, Ord)

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
                             , basicBlockInstructions :: Vector Instruction
                             , basicBlockPhiCount :: Int
                             , basicBlockSuccessors :: [BasicBlock]
                             , basicBlockPredecessors :: [BasicBlock]
                             }

instance Eq BasicBlock where
  (==) = (==) `on` basicBlockId

instance Ord BasicBlock where
  compare = compare `on` basicBlockId

instance Hashable BasicBlock where
  hashWithSalt s = hashWithSalt s . basicBlockId

type UniqueId = Int

data Instruction = Return { instructionId :: UniqueId
                          , instructionType :: Type
                          , returnValue :: Maybe Value
                          }
                 | MoveException { instructionId :: UniqueId
                                 , instructionType :: Type
                                 }
                 | MonitorEnter { instructionId :: UniqueId
                                , instructionType :: Type
                                , monitorReference :: Value
                                }
                 | MonitorExit { instructionId :: UniqueId
                               , instructionType :: Type
                               , monitorReference :: Value
                               }
                 | CheckCast { instructionId :: UniqueId
                             , instructionType :: Type
                             , castReference :: Value
                             , castType :: Type
                             }
                 | InstanceOf { instructionId :: UniqueId
                              , instructionType :: Type
                              , instanceOfReference :: Value
                              }
                 | ArrayLength { instructionId :: UniqueId
                               , instructionType :: Type
                               , arrayReference :: Value
                               }
                 | NewInstance { instructionId :: UniqueId
                               , instructionType :: Type
                               }
                 | NewArray { instructionId :: UniqueId
                            , instructionType :: Type
                            , newArrayLength :: Value
                            , newArrayContents :: Maybe [Value]
                            }
                 | FillArray { instructionId :: UniqueId
                             , instructionType :: Type
                             , fillArrayReference :: Value
                             , fillArrayContents :: [Int64]
                             }
                 | Throw { instructionId :: UniqueId
                         , instructionType :: Type
                         , throwReference :: Value
                         }
                 | ConditionalBranch { instructionId :: UniqueId
                                     , instructionType :: Type
                                     , branchOperand1 :: Value
                                     , branchOperand2 :: Value
                                     , branchTestType :: LL.IfOp
                                     , branchTarget :: BasicBlock
                                     , branchFallthrough :: BasicBlock
                                     }
                 | UnconditionalBranch { instructionId :: UniqueId
                                       , instructionType :: Type
                                       , branchTarget :: BasicBlock
                                       }
                 | Switch { instructionId :: UniqueId
                          , instructionType :: Type
                          , switchValue :: Value
                          , switchTargets :: [(Int64, BasicBlock)]
                          , switchFallthrough :: BasicBlock
                          }
                 | Compare { instructionId :: UniqueId
                           , instructionType :: Type
                           , compareOperation :: LL.CmpOp
                           , compareOperand1 :: Value
                           , compareOperand2 :: Value
                           }
                 | UnaryOp { instructionId :: UniqueId
                           , instructionType :: Type
                           , unaryOperand :: Value
                           , unaryOperation :: LL.Unop
                           }
                 | BinaryOp { instructionId :: UniqueId
                            , instructionType :: Type
                            , binaryOperand1 :: Value
                            , binaryOperand2 :: Value
                            , binaryOperation :: LL.Binop
                            }
                 | ArrayGet { instructionId :: UniqueId
                            , instructionType :: Type
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            }
                 | ArrayPut { instructionId :: UniqueId
                            , instructionType :: Type
                            , arrayReference :: Value
                            , arrayIndex :: Value
                            , arrayPutValue :: Value
                            }
                 | StaticGet { instructionId :: UniqueId
                             , instructionType :: Type
                             , staticOpField :: Field
                             }
                 | StaticPut { instructionId :: UniqueId
                             , instructionType :: Type
                             , staticOpField :: Field
                             , staticOpPutValue :: Value
                             }
                 | InstanceGet { instructionId :: UniqueId
                               , instructionType :: Type
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               }
                 | InstancePut { instructionId :: UniqueId
                               , instructionType :: Type
                               , instanceOpReference :: Value
                               , instanceOpField :: Field
                               , instanceOpPutValue :: Value
                            }
                 | InvokeVirtual { instructionId :: UniqueId
                                 , instructionType :: Type
                                 , invokeVirtualKind :: InvokeVirtualKind
                                 , invokeVirtualMethod :: MethodRef
                                 , invokeArguments :: [Value]
                                 }
                 | InvokeDirect { instructionId :: UniqueId
                                , instructionType :: Type
                                , invokeDirectKind :: InvokeDirectKind
                                , invokeDirectMethod :: MethodRef
                                , invokeDirectMethodDef :: Maybe Method
                                , invokeArguments :: [Value]
                                }
                   -- ^ We have to refer to the invoked method in this
                   -- case by a Ref because it might not have a
                   -- definition in this dex file.  However, we do
                   -- include the definition of the invoked method if
                   -- it is available.
                 | Phi { instructionId :: UniqueId
                       , instructionType :: Type
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

instance FromValue Instruction where
  fromValue (InstructionV i) = return i
  fromValue _ = failure $ CastException "Not an Instruction"

data Parameter = Parameter { parameterId :: UniqueId
                           , parameterType :: Type
                           , parameterName :: String
                           , parameterIndex :: Int
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

instance FromValue Parameter where
  fromValue (ParameterV p) = return p
  fromValue _ = failure $ CastException "Not a Parameter"

data Method = Method { methodId :: UniqueId
                     , methodName :: String
                     , methodReturnType :: Type
                     , methodAccessFlags :: AccessFlags
                     , methodParameters :: [Parameter]
                     , methodBody :: Maybe [BasicBlock]
                     }

instance Eq Method where
  (==) = (==) `on` methodId

instance Ord Method where
  compare = compare `on` methodId

instance Hashable Method where
  hashWithSalt s = hashWithSalt s . methodId

data Class = Class { classId :: UniqueId
                   , className :: String
                   , classAccessFlags :: AccessFlags
                   , classParent :: Maybe Type
                   , classParentReference :: Maybe Class
                   , classInterfaces :: [Type]
                   , classDirectMethods :: [Method]
                   , classVirtualMethods :: [Method]
                   , classStaticFields :: [(AccessFlags, Field)]
                   , classInstanceFields :: [(AccessFlags, Field)]
                   }

instance Eq Class where
  (==) = (==) `on` classId

instance Ord Class where
  compare = compare `on` classId

instance Hashable Class where
  hashWithSalt s = hashWithSalt s . classId

data Field = Field { fieldId :: UniqueId
                   , fieldName :: String
                   , fieldType :: Type
                   , fieldClass :: Type
                   }

-- | Field IDs are unique among fields (but could overlap with IDs of
-- other types).
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
                           , methodRefName :: String
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

