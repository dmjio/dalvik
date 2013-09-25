module Dalvik.SSA.Types --  (
  -- Instruction(..),
  -- LL.CmpOp(..),
  -- ) where
  where

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

-- FIXME: These will need to be adjusted to reflect the actual
-- primitives available...  Unfortunately, typing them will be
-- difficult.
data Constant = ConstantInt !Int !Int64
              | ConstantDouble !Int !Double
              | ConstantString !Int String
              | ConstantBoolean !Int !Bool
              | ConstantChar !Int !Char
              | ConstantNull !Int

constantId :: Constant -> Int
constantId (ConstantInt i _) = i
constantId (ConstantDouble i _) = i
constantId (ConstantString i _) = i
constantId (ConstantBoolean i _) = i
constantId (ConstantChar i _) = i
constantId (ConstantNull i) = i

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
                                 , invokeVirtualMethod :: InvokeVirtualMethod
                                 , invokeArguments :: [Value]
                                 }
                 | InvokeDirect { instructionId :: UniqueId
                                , instructionType :: Type
                                , invokeDirectMethod :: InvokeDirectMethod
                                , invokeArguments :: [Value]
                                }
                 | Phi { instructionId :: UniqueId
                       , instructionType :: Type
                       , phiValues :: [(BasicBlock, Value)]
                       }

data Parameter = Parameter { parameterId :: UniqueId
                           , parameterType :: Type
                           , parameterName :: Maybe String
                           , parameterIndex :: Int
                           }

data Method = Method { methodId :: UniqueId
                     , methodName :: String
                     , methodReturnType :: Type
                     , methodAccessFlags :: AccessFlags
                     , methodParameters :: [Parameter]
                     , methodBody :: Maybe [BasicBlock]
                     }

data Interface = Interface { interfaceId :: UniqueId
                           , interfaceName :: String
                           , interfaceMethods :: [Method]
                           }

data Class = Class { classId :: UniqueId
                   , className :: String
                   , classParent :: Maybe Class
                   , classInterfaces :: [Interface]
                   , classDirectMethods :: [Method]
                   , classVirtualMethods :: [Method]
                   , classStaticFields :: [Field]
                   , classInstanceFields :: [Field]
                   }

data Field = Field { fieldId :: UniqueId
                   , fieldName :: String
                   , fieldType :: Type
                   , fieldAccessFlags :: AccessFlags
                   , fieldClass :: Class
                   }

-- | Field IDs are unique among fields (but could overlap with IDs of
-- other types).
instance Eq Field where
  (==) = (==) `on` fieldId

instance Ord Field where
  compare = compare `on` fieldId

data MethodRef = ClassMethodRef Class Int
               | InterfaceMethodRef Interface Int

data InvokeDirectMethod = MethodInvokeStatic
                        | MethodInvokeDirect

data InvokeVirtualMethod = MethodInvokeInterface
                         | MethodInvokeSuper
                         | MethodInvokeVirtual
