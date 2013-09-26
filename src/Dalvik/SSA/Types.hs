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
  valueId,
  valueType,
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
  module Dalvik.AccessFlags
  ) where

import Data.Function ( on )
import Data.Int ( Int64 )
import Data.Vector ( Vector )

import Dalvik.AccessFlags
import Dalvik.ClassHierarchy
-- Low-level instructions
import qualified Dalvik.Instruction as LL

data DexFile =
  DexFile { dexIdentifier :: String
          , dexClasses :: [Class]
          }

data Value = InstructionV Instruction
           | ConstantV Constant
           | ParameterV Parameter

-- FIXME: For now, all numeric constants are integer types because we
-- can't tell (without a type inference pass) what type a constant
-- really is.  Dalvik is untyped.
data Constant = ConstantInt !UniqueId !Int64
              | ConstantString !UniqueId String
              | ConstantClass !UniqueId Type

constantId :: Constant -> Int
constantId (ConstantInt i _) = i
constantId (ConstantString i _) = i
constantId (ConstantClass i _) = i

constantType :: Constant -> Type
constantType _ = UnknownType

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
          | ReferenceType ClassName -- Class
          | UnknownType
            -- ^ We use this in cases where we can't deduce a type
            -- during the SSA translation
          deriving (Eq, Ord, Show)

valueId :: Value -> UniqueId
valueId (InstructionV i) = instructionId i
valueId (ConstantV c) = constantId c
valueId (ParameterV p) = parameterId p

valueType :: Value -> Type
valueType (InstructionV i) = instructionType i
valueType (ConstantV c) = constantType c
valueType (ParameterV p) = parameterType p

-- | A basic block containing 'Instruction's.  We maintain a count of
-- the phi nodes in the block so that we can efficiently slice out
-- either instructions or phi nodes for separate processing.
data BasicBlock = BasicBlock { basicBlockId :: UniqueId
                             , basicBlockInstructions :: Vector Instruction
                             , basicBlockPhiCount :: Int
                             }

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

data Parameter = Parameter { parameterId :: UniqueId
                           , parameterType :: Type
                           , parameterName :: String
                           , parameterIndex :: Int
                           }

data Method = Method { methodId :: UniqueId
                     , methodName :: String
                     , methodReturnType :: Type
                     , methodAccessFlags :: AccessFlags
                     , methodParameters :: [Parameter]
                     , methodBody :: Maybe [BasicBlock]
                     }

data Class = Class { classId :: UniqueId
                   , className :: String
                   , classParent :: Maybe Class
                   , classInterfaces :: [Type]
                   , classDirectMethods :: [Method]
                   , classVirtualMethods :: [Method]
                   , classStaticFields :: [(AccessFlags, Field)]
                   , classInstanceFields :: [(AccessFlags, Field)]
                   }

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

data MethodRef = MethodRef { methodRefId :: UniqueId
                           , methodRefClass :: Type
                           , methodRefReturnType :: Type
                           , methodRefParameterTypes :: [Type]
                           , methodRefName :: String
                           }


data InvokeDirectKind = MethodInvokeStatic
                      | MethodInvokeDirect

data InvokeVirtualKind = MethodInvokeInterface
                       | MethodInvokeSuper
                       | MethodInvokeVirtual
