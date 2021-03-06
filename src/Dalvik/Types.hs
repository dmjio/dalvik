{-# LANGUAGE DeriveDataTypeable #-}
module Dalvik.Types where

import Control.Monad.Catch as E
import Control.Monad ( guard )
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Int
import qualified Data.List as L
import Data.Map (Map)
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Typeable
import Data.Word
import Text.Printf

import Prelude

import Dalvik.AccessFlags

data DecodeError = PrematureEnd Word8 Word16
                 | InvalidOpcode Word8
                 | InvalidBForIF35cEncoding Word8
                 | NoStringAtIndex StringId
                 | NoTypeAtIndex TypeId
                 | NoFieldAtIndex FieldId
                 | NoMethodAtIndex MethodId
                 | NoProtoAtIndex ProtoId
                 | NoClassAtIndex TypeId
                 | NoCodeForMethod MethodId
                 | NoHandlerAtOffset Word32
                 | TypeDecodeError String BS.ByteString
                 | NonPhiLabelInBlockHeader String
                 | InvalidArrayDataList Int [Word16]
                 | InvalidArrayDataElementSize Word16
                 | NoArrayDataForFillArray Int
                 | NoLabelForExpectedRegister String Word16 Int
                 | NoParameterAtIndex MethodId Int
                 | NonArgumentLabelInParameterList MethodId String
                 | ArgumentTypeMismatch MethodId [Reg16]
                 | ClassAlreadyDefined String
                 | NoReceiverForVirtualCall String
                 | EmptyBlockWithNonSingletonSuccessorList Int
                 deriving (Eq, Ord, Show, Typeable)

instance Exception DecodeError

decodeErrorAsString :: DecodeError -> String
decodeErrorAsString (PrematureEnd opcode w) =
  printf "Premature end of data stream at opcode %02x (%04x)" opcode w
decodeErrorAsString (InvalidOpcode op) =
  "Invalid opcode " ++ show op
decodeErrorAsString (InvalidBForIF35cEncoding b) =
  printf "Invalid b value (%d) for IF35c encoding" b
decodeErrorAsString (NoStringAtIndex i) =
  printf "No string at index %d" i
decodeErrorAsString (NoTypeAtIndex i) =
  printf "No type at index %d" i
decodeErrorAsString (NoFieldAtIndex i) =
  printf "No field at index %d" i
decodeErrorAsString (NoMethodAtIndex i) =
  printf "No method at index %d" i
decodeErrorAsString (NoProtoAtIndex i) =
  printf "No prototype at index %d" i
decodeErrorAsString (NoClassAtIndex i) =
  printf "No class at index %d" i
decodeErrorAsString (NoCodeForMethod mId) =
  printf "No code for method %d" mId
decodeErrorAsString (NoHandlerAtOffset i) =
  printf "No exception handling block at offset %d" i
decodeErrorAsString (TypeDecodeError reason bs) =
  printf "Error decoding type name (%s): %s" (show bs) reason
decodeErrorAsString (NonPhiLabelInBlockHeader s) =
  printf "Non Phi label in basic block header: %s" s
decodeErrorAsString (InvalidArrayDataList eltSz ws) =
  printf "Could not decode array data list (%d bytes per element): %s" eltSz (show ws)
decodeErrorAsString (InvalidArrayDataElementSize esz) =
  printf "Invalid array data element size in array-data-payload: %d" esz
decodeErrorAsString (NoArrayDataForFillArray ix) =
  printf "No array-data-payload for fill-array at index %d" ix
decodeErrorAsString (NoLabelForExpectedRegister loc regNo ix) =
  printf "Missing SSA label for expected %s register %s at %s" loc regNo ix
decodeErrorAsString (NoParameterAtIndex mix ix) =
  printf "No parameter at expected argument list index %d in method %d" ix mix
decodeErrorAsString (NonArgumentLabelInParameterList mix lbl) =
  printf "Non-argument typed label (%s) in argument label list for method %d" lbl mix
decodeErrorAsString (ArgumentTypeMismatch mId argRegs) =
  printf "Wide argument with not enough registers remaining (%s) in call to %d" (show argRegs) mId
decodeErrorAsString (ClassAlreadyDefined s) =
  printf "Class %s was already defined in another apk or dex file" s
decodeErrorAsString (NoReceiverForVirtualCall s) =
  printf "No receiver for virtual call: %s" s
decodeErrorAsString (EmptyBlockWithNonSingletonSuccessorList bid) =
  printf "Empty block with a non-singleton successor list: %d" bid

data DexHeader =
  DexHeader
  { dexMagic        :: BS.ByteString
  , dexVersion      :: BS.ByteString
  , dexChecksum     :: Word32
  , dexSHA1         :: [Word8]
  , dexFileLen      :: Word32
  , dexHdrLen       :: Word32
  , dexLinkSize     :: Word32
  , dexLinkOff      :: Word32
  , dexMapOff       :: Word32
  , dexNumStrings   :: Word32
  , dexOffStrings   :: Word32
  , dexNumTypes     :: Word32
  , dexOffTypes     :: Word32
  , dexNumProtos    :: Word32
  , dexOffProtos    :: Word32
  , dexNumFields    :: Word32
  , dexOffFields    :: Word32
  , dexNumMethods   :: Word32
  , dexOffMethods   :: Word32
  , dexNumClassDefs :: Word32
  , dexOffClassDefs :: Word32
  , dexDataSize     :: Word32
  , dexDataOff      :: Word32
  } deriving (Show)

type ProtoId = Word16
type ParamListId = Word32
type FieldId = Word16
type MethodId = Word16
type StringId = Word32
type StringIdJumbo = Word32
type TypeId = Word16

type Word4 = Word8

type Reg4 = Word4
type Reg8 = Word8
type Reg16 = Word16


data MapItem
  = MapItem {
      itemType :: Word16
    , itemSize :: Word32
    , itemOff  :: Word32
    } deriving (Show)

data Field
  = Field {
      fieldClassId :: TypeId
    , fieldTypeId  :: TypeId
    , fieldNameId  :: StringId
    } deriving (Show)

data EncodedField
  = EncodedField {
      fieldId :: FieldId
    , fieldAccessFlags :: AccessFlags
    } deriving (Show)

data Proto
  = Proto {
      protoShortDesc :: StringId
    , protoRet       :: TypeId
    , protoParams    :: [TypeId]
    } deriving (Show)

data Method
  = Method {
      methClassId  :: TypeId
    , methProtoId  :: ProtoId
    , methNameId   :: StringId
    } deriving (Show)

data EncodedMethod
  = EncodedMethod {
      methId :: MethodId
    , methAccessFlags :: AccessFlags
    , methCode :: Maybe CodeItem
    } deriving (Show)

data Class
  = Class
    { classId :: TypeId
    , classAccessFlags :: AccessFlags
    , classSuperId :: TypeId
    , classInterfacesOff :: Word32
    , classInterfaces :: [TypeId]
    , classSourceNameId :: StringId
    , classAnnotsOff :: Word32
    , classStaticFields :: [EncodedField]
    , classInstanceFields :: [EncodedField]
    , classDirectMethods :: [EncodedMethod]
    , classVirtualMethods :: [EncodedMethod]
    , classDataOff :: Word32
    , classStaticValuesOff :: Word32
    , classAnnotations :: ClassAnnotations
    } deriving (Show)

data EncodedValue = EncodedByte Int8
                  | EncodedShort Int16
                  | EncodedChar Char
                  | EncodedInt Int32
                  | EncodedLong Int64
                  | EncodedFloat Float
                  | EncodedDouble Double
                  | EncodedStringRef StringId
                  | EncodedTypeRef TypeId
                  | EncodedFieldRef FieldId
                  | EncodedMethodRef MethodId
                  | EncodedEnumRef FieldId
                  | EncodedArray [EncodedValue]
                  | EncodedAnnotation Annotation
                  | EncodedNull
                  | EncodedBool Bool
                  deriving (Show)

data AnnotationVisibility = AVBuild
                            -- ^ Discarded after the build
                          | AVRuntime
                            -- ^ Available to the application at run time
                          | AVSystem
                            -- ^ Available to the system at run time
                          deriving (Eq, Ord, Show)

data Annotation
  = Annotation
    { annotTypeId :: TypeId
    , annotElements :: [(StringId, EncodedValue)]
    }
  deriving (Show)

data VisibleAnnotation
  = VisibleAnnotation
    { vaVisibility :: AnnotationVisibility
    , vaAnnotation :: Annotation
    }
  deriving (Show)

data ClassAnnotations
  = ClassAnnotations
    { classAnnotationsAnnot :: [VisibleAnnotation]
    , classFieldAnnotations :: [(FieldId, [VisibleAnnotation])]
    , classMethodAnnotations :: [(MethodId, [VisibleAnnotation])]
    , classParamAnnotations :: [(MethodId, [[VisibleAnnotation]])]
    }
  deriving (Show)

noClassAnnotations :: ClassAnnotations
noClassAnnotations = ClassAnnotations { classAnnotationsAnnot = []
                                      , classFieldAnnotations = []
                                      , classMethodAnnotations = []
                                      , classParamAnnotations = []
                                      }

data TryItem
  = TryItem
    { tryStartAddr  :: Word32
    , tryInsnCount  :: Word16
    , tryHandlerOff :: Word16
    } deriving (Show)

data CatchHandler
  = CatchHandler
    { chHandlerOff :: Word32
    , chHandlers   :: [(TypeId, Word32)]
    , chAllAddr    :: Maybe Word32
    } deriving (Show)

data CodeItem
  = CodeItem
    { codeRegs      :: Word16
    , codeInSize    :: Word16
    , codeOutSize   :: Word16
    , codeDebugInfo :: Maybe DebugInfo
    , codeInsnOff   :: Word32
    , codeInsns     :: [Word16]
    , codeTryItems  :: [TryItem]
    , codeHandlers  :: [CatchHandler]
    } deriving (Show)

data DexFile =
  DexFile
  { dexHeader       :: DexHeader
  , dexMap          :: Map Word32 MapItem
  , dexStrings      :: Map StringId BS.ByteString
  , dexTypeNames    :: Map TypeId StringId
  , dexProtos       :: Map ProtoId Proto
  , dexFields       :: Map FieldId Field
  , dexMethods      :: Map MethodId Method
  , dexClasses      :: Map TypeId Class
  , dexThisId       :: StringId
  } deriving (Show)

data DebugByteCode
  = DBG_END_SEQUENCE
  | DBG_ADVANCE_PC
  | DBG_ADVANCE_LINE
  | DBG_START_LOCAL
  | DBG_START_LOCAL_EXTENDED
  | DBG_END_LOCAL
  | DBG_RESTART_LOCAL
  | DBG_SET_PROLOGUE_END
  | DBG_SET_EPILOGUE_BEGIN
  | DBG_SET_FILE
  | DBG_FIRST_SPECIAL
    deriving (Eq, Enum)

data DebugInstruction
  = EndSequence
  | AdvancePC Word32
  | AdvanceLine Int32
  | StartLocal Word32 Int32 Int32 --(Maybe Word32) (Maybe Word32)
  | StartLocalExt Word32 Int32 Int32 Int32 --(Maybe Word32) (Maybe Word32) (Maybe Word32)
  | EndLocal Word32
  | RestartLocal Word32
  | SetPrologueEnd
  | SetEpilogueBegin
  | SetFile Int32 --(Maybe Word32)
  | SpecialAdjust Word8
    deriving (Show)

data DebugInfo
  = DebugInfo
    { dbgLineStart  :: Word32
    , dbgParamNames :: [Int32]
    , dbgByteCodes  :: [DebugInstruction]
    } deriving (Show)

data DebugState
  = DebugState
    { dbgAddr          :: Word32
    , dbgLine          :: Word32
    , dbgSourceFile    :: Int32
    , dbgPrologueEnd   :: Bool
    , dbgEpilogueBegin :: Bool
    , dbgLocals        :: Map Word32 [LocalInfo]
    , dbgPositions     :: [PositionInfo]
    , dbgSeqNo         :: Word32
    } deriving (Show)

data PositionInfo
  = PositionInfo
    { pAddr :: Word32
    , pLine :: Word32
    } deriving (Show)

data LocalInfo
  = LocalInfo
    { lSeqNo     :: Word32
    , lStartAddr :: Word32
    , lEndAddr   :: Word32
    , lNameID    :: Int32
    , lTypeID    :: Int32
    , lTypeSig   :: Int32
    } deriving (Eq, Ord, Show)

{- Utility functions -}

getStr :: (E.MonadThrow m) => DexFile -> StringId -> m BS.ByteString
getStr dex i =
  maybe (E.throwM (NoStringAtIndex i)) return $ Map.lookup i (dexStrings dex)

getTypeName :: (E.MonadThrow f) => DexFile -> TypeId -> f BS.ByteString
getTypeName dex i =
  maybe (E.throwM (NoTypeAtIndex i)) (getStr dex) $ Map.lookup i (dexTypeNames dex)

getField :: (E.MonadThrow f) => DexFile -> FieldId -> f Field
getField dex i =
  maybe (E.throwM (NoFieldAtIndex i)) return $ Map.lookup i (dexFields dex)

getMethod :: (E.MonadThrow f) => DexFile -> MethodId -> f Method
getMethod dex i =
  maybe (E.throwM (NoMethodAtIndex i)) return $ Map.lookup i (dexMethods dex)

getProto :: (E.MonadThrow f) => DexFile -> ProtoId -> f Proto
getProto dex i =
  maybe (E.throwM (NoProtoAtIndex i)) return $ Map.lookup i (dexProtos dex)

getClass :: (E.MonadThrow f) => DexFile -> TypeId -> f Class
getClass dex i =
  maybe (E.throwM (NoClassAtIndex i)) return $ Map.lookup i (dexClasses dex)

findString :: DexFile -> BS.ByteString -> Maybe StringId
findString dex t =
  case filter isThis (Map.toList (dexStrings dex)) of
    [(sid, _)] -> Just sid
    _ -> Nothing
  where isThis (_, t') = t == t'

isStatic :: EncodedMethod -> Bool
isStatic (EncodedMethod _ flags _) = hasAccessFlag ACC_STATIC flags

getEncodedMethod :: DexFile -> String -> String -> String -> Maybe EncodedMethod
getEncodedMethod dx className methodName typeSig = do
  let classes = Map.elems $ dexClasses dx
  c <- L.find (maybe False (==fromString className) . getTypeName dx . classId) classes
  let ms = classDirectMethods c ++ classVirtualMethods c
  L.find checkMethod ms
  where
    checkMethod encMeth = fromMaybe False $ do
      m <- getMethod dx (methId encMeth)
      mname <- getStr dx (methNameId m)
      guard (mname == fromString methodName)
      sig <- encodedMethodSignature dx encMeth
      let msig = methodSignatureString sig
      -- p <- getProto dx (methProtoId m)
      -- rstr <- getTypeName dx (protoRet p)
      -- pstrs <- mapM (getTypeName dx) (protoParams p)
      -- let msig = mconcat [ fromString "(", mconcat pstrs, fromString ")", rstr ]
      return $ fromString typeSig == msig

encodedMethodSignature :: DexFile -> EncodedMethod -> Maybe ([BS.ByteString], BS.ByteString)
encodedMethodSignature df em = do
  m <- getMethod df (methId em)
  p <- getProto df (methProtoId m)
  rtype <- getTypeName df (protoRet p)
  ptypes <- mapM (getTypeName df) (protoParams p)
  return (ptypes, rtype)

methodSignatureString :: ([BS.ByteString], BS.ByteString) -> BS.ByteString
methodSignatureString (pts, rt) =
  mconcat [ fromString "(", mconcat pts, fromString ")", rt ]
