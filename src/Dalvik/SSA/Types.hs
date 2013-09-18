module Dalvik.SSA.Types --  (
  -- Instruction(..),
  -- LL.CmpOp(..),
  -- ) where
  where

import Data.Int ( Int64 )
import Data.Vector ( Vector )

-- Low-level instructions
import qualified Dalvik.Instruction as LL

data DexFile = DexFile { dexClasses :: [Class] }

data Value = InstructionV Instruction
           | ConstantV Constant
           | ParameterV Parameter

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
          | ReferenceType String

valueId :: Value -> UniqueId
valueId (InstructionV i) = instructionId i
valueId (ConstantV c) = constantId c
valueId (ParameterV p) = parameterId p

data BasicBlock = BasicBlock { basicBlockId :: UniqueId
                             , blockInstructions :: Vector Instruction
                             }

type UniqueId = Int

data Instruction = Return { instructionId :: UniqueId
                          , instructionType :: Type
                          , returnValue :: Maybe Value
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
                              , instanceOfType :: Type
                              }
                 | ArrayLength { instructionId :: UniqueId
                               , instructionType :: Type
                               , arrayReference :: Value
                               }
                 | NewInstance { instructionId :: UniqueId
                               , instructionType :: Type
                               , newInstanceType :: Type
                               }
                 | NewArray { instructionId :: UniqueId
                            , instructionType :: Type
                            , newArrayType :: Type
                            , newArrayLength :: Value
                            , newArrayContents :: Maybe [Value]
                            }
                 | Throw { instructionId :: UniqueId
                         , instructionType :: Type
                         , throwReference :: Value
                         }
                 | ConditionalBranch { instructionId :: UniqueId
                                     , instructionType :: Type
                                     , branchPredicate :: Value
                                     , branchTarget :: BasicBlock
                                     }
                 | UnconditionalBranch { instructionId :: UniqueId
                                       , instructionType :: Type
                                       , branchTarget :: BasicBlock
                                       }
                 | Switch { instructionId :: UniqueId
                          , instructionType :: Type
                          , switchValue :: Value
                          , switchTargets :: [(Int64, BasicBlock)]
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
                             , staticClass :: Type
                             , staticFieldName :: String
                             , staticFieldNumber :: Int
                             }
                 | StaticPut { instructionId :: UniqueId
                             , instructionType :: Type
                             , staticClass :: Type
                             , staticFieldName :: String
                             , staticFieldNumber :: Int
                             , staticPutValue :: Value
                             }
                 | FieldGet { instructionId :: UniqueId
                            , instructionType :: Type
                            , fieldReference :: Value
                            , fieldName :: String
                            , fieldNumber :: Int
                            }
                 | FieldPut { instructionId :: UniqueId
                            , instructionType :: Type
                            , fieldReference :: Value
                            , fieldName :: String
                            , fieldNumber :: Int
                            , fieldPutValue :: Value
                            }
                 | Invoke { instructionId :: UniqueId
                          , instructionType :: Type
                          , invokeMethod :: InvokeMethod
                          , invokeArguments :: [Value]
                          }
                 | Phi { instructionId :: UniqueId
                       , instructionType :: Type
                       , phiValues :: [(BasicBlock, Value)]
                       }

data Parameter = Parameter { parameterId :: UniqueId
                           , parameterType :: Type
                           , parameterName :: String
                           }

data Method = Method { methodId :: UniqueId
                     , methodName :: String
                     , methodType :: Type
                     , methodParameters :: [Parameter]
                     , methodBody :: [BasicBlock]
                     }
            | AbstractMethod { methodId :: UniqueId
                             , methodName :: String
                             , methodType :: Type
                             }

data Interface = Interface { interfaceId :: UniqueId
                           , interfaceName :: String
                           , interfaceMethods :: [Method]
                           }

data Class = Class { classId :: UniqueId
                   , className :: String
                   , classParent :: Maybe Class
                   , classInterfaces :: [Interface]
                   , classMethods :: [Method]
                   , classMembers :: [Member]
                   }

data Member = Member { memberId :: UniqueId
                     , memberName :: String
                     , memberType :: Type
                     }

data MethodRef = ClassMethodRef Class Int
               | InterfaceMethodRef Interface Int

data InvokeMethod = InvokeInterface MethodRef
                  | InvokeVirtual MethodRef
                  | InvokeSuper MethodRef
                  | InvokeStatic Method
                  | InvokeDirect Method
