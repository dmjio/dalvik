{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dalvik.Printer
  ( prettyInstruction
  , prettyHeader
  , prettyClassDef
  , protoDesc
  , methodStr
  , getStr'
  , getTypeName'
  ) where

import qualified Control.Exception as E
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.Int
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe )
import qualified Data.Monoid as M
import Data.Serialize.Get ( runGet )
import Data.Serialize.Put ( runPut, putWord32le, putWord64le )
import Data.Serialize.IEEE754 ( getFloat32le, getFloat64le )
import Data.Word
import Text.FShow.RealFloat
import Text.PrettyPrint.HughesPJClass as PP
import Text.Printf ( printf )

import Prelude

import qualified Dalvik.AccessFlags as AF
import qualified Dalvik.DebugInfo as DI
import Dalvik.Instruction
import Dalvik.Types

methodComm :: MethodId -> PP.Doc
methodComm mid = " // method@" <> word16HexFixed mid

typeComm :: TypeId -> PP.Doc
typeComm tid = " // type@" <> word16HexFixed tid

fieldComm :: FieldId -> PP.Doc
fieldComm fid = " // field@" <> word16HexFixed fid

stringComm :: StringId -> PP.Doc
stringComm sid = " // string@" <> word16HexFixed (fromIntegral sid)

intComm8 :: Word8 -> PP.Doc
intComm8 i = " // #" <> word8Hex i

intComm8' :: Word8 -> PP.Doc
intComm8' i = " // #" <> word8HexFixed i

intComm16 :: Word16 -> PP.Doc
intComm16 i = " // #" <> word16Hex i

intComm16' :: Word16 -> PP.Doc
intComm16' i = " // #" <> word16HexFixed i

intComm32 :: Word32 -> PP.Doc
intComm32 i = " // #" <> word32HexFixed i

intComm64 :: Word64 -> PP.Doc
intComm64 i = " // #" <> word64HexFixed i

offComm :: Int16 -> PP.Doc
offComm i = " // " <> sign <> int16HexFixed i'
  where (sign, i') | i < 0 = ("-", -i)
                   | otherwise = ("+", i)

offComm32 :: Int32 -> PP.Doc
offComm32 i = " // " <> int32HexFixed i

offComm' :: Word32 -> PP.Doc
offComm' i = " // " <> sign <> word32HexFixed i'
  where (sign, i') | i < 0 = ("-", -i)
                   | otherwise = ("+", i)

regStr :: Reg -> PP.Doc
regStr (R4 r) = iregStr8 r
regStr (R8 r) = iregStr8 r
regStr (R16 r) = iregStr16 r

iregStr8 :: Word8 -> PP.Doc
iregStr8 r = "v" <> word8Dec r

iregStr16 :: Word16 -> PP.Doc
iregStr16 r = "v" <> word16Dec r

moveTypeString :: MoveType -> PP.Doc
moveTypeString MNormal = "move"
moveTypeString MWide = "move-wide"
moveTypeString MObject = "move-object"

moveSizeString :: Reg -> PP.Doc
moveSizeString (R4 _) = ""
moveSizeString (R8 _) = "/from16"
moveSizeString (R16 _) = "/16"

constStr :: DexFile -> PP.Doc -> Reg -> ConstArg -> PP.Doc
constStr dex instr d c = mkInsn instr [regStr d, constString dex c]

ifOpStr :: IfOp -> PP.Doc
ifOpStr Eq = "eq"
ifOpStr Ne = "ne"
ifOpStr Lt = "lt"
ifOpStr Ge = "ge"
ifOpStr Gt = "gt"
ifOpStr Le = "le"

typeStr :: CType -> PP.Doc
typeStr Byte = "byte"
typeStr Char = "char"
typeStr Short = "short"
typeStr Int = "int"
typeStr Long = "long"
typeStr Float = "float"
typeStr Double = "double"

unopStr :: Unop -> PP.Doc
unopStr NegInt = "neg-int"
unopStr NotInt = "not-int"
unopStr NegLong = "neg-long"
unopStr NotLong = "not-long"
unopStr NegFloat = "neg-float"
unopStr NegDouble = "neg-double"
unopStr (Convert ty1 ty2) = M.mconcat [typeStr ty1, "-to-",  typeStr ty2]

binopStr :: Binop -> PP.Doc
binopStr Add = "add"
binopStr Sub = "sub"
binopStr Mul = "mul"
binopStr Div = "div"
binopStr Rem = "rem"
binopStr And = "and"
binopStr Or = "or"
binopStr Xor = "xor"
binopStr Shl = "shl"
binopStr Shr = "shr"
binopStr UShr = "ushr"
binopStr RSub = "rsub"

int32ToFloat :: Int32 -> Float
int32ToFloat =
  either (const (0/0)) id .
  runGet getFloat32le .
  runPut .
  putWord32le .
  fromIntegral

int64ToDouble :: Int64 -> Double
int64ToDouble =
  either (const (0/0)) id .
  runGet getFloat64le .
  runPut .
  putWord64le .
  fromIntegral

ffmt :: Float -> PP.Doc
ffmt f | isNaN f = "nan"
       | otherwise = PP.text $ fshowFFloat (Just 6) (FF f) ""

dfmt :: Double -> PP.Doc
dfmt d | isNaN d = "nan"
       | otherwise = PP.text $ fshowFFloat (Just 6) (FD d) ""

constString :: DexFile -> ConstArg -> PP.Doc
constString _ (Const4 i) =
  "#int " <> int8Dec (fromIntegral i) <> intComm8 (fromIntegral i)
constString _ (Const16 i) =
  "#int " <> int16Dec (fromIntegral i) <> intComm16 (fromIntegral i)
constString _ (ConstHigh16 i) =
  "#int " <> int32Dec i <> intComm16 (fromIntegral (i `shiftR` 16) :: Word16)
constString _ (ConstWide16 i) =
  "#int " <> int64Dec i <> intComm16 (fromIntegral i)
constString _ (Const32 w) =
  -- dexdump always prints these as floats, even though they might not be.
  "#float " <> ffmt (int32ToFloat w) <> intComm32 (fromIntegral w :: Word32)
constString _ (ConstWide32 i) =
  -- dexdump always prints these as floats, even though they might not be.
  "#float " <> ffmt (int32ToFloat (fromIntegral i)) <> intComm32 (fromIntegral i)
constString _ (ConstWide w) =
  -- dexdump always prints these as doubles, even though they might not be.
  "#double " <> dfmt (int64ToDouble w) <> intComm64 (fromIntegral w)
constString _ (ConstWideHigh16 i) =
  "#long " <> int64Dec i <> intComm16 (fromIntegral (i `shiftR` 48) :: Word16)
constString dex (ConstString sid) =
  "\"" <> getStr' dex sid <> "\"" <> stringComm sid
constString dex (ConstStringJumbo sid) =
  "\"" <> getStr' dex sid <> "\"" <> stringComm sid
constString dex (ConstClass tid) =
  getTypeName' dex tid <> typeComm tid

accessOpStr :: AccessOp -> PP.Doc
accessOpStr (Get ty) = "get" <> accessTypeStr ty
accessOpStr (Put ty) = "put" <> accessTypeStr ty

accessTypeStr :: Maybe AccessType -> PP.Doc
accessTypeStr Nothing = ""
accessTypeStr (Just AWide) = "-wide"
accessTypeStr (Just AObject) = "-object"
accessTypeStr (Just ABoolean) = "-boolean"
accessTypeStr (Just AByte) = "-byte"
accessTypeStr (Just AChar) = "-char"
accessTypeStr (Just AShort) = "-short"

ikindStr :: InvokeKind -> PP.Doc
ikindStr Virtual = "virtual"
ikindStr Super = "super"
ikindStr Direct = "direct"
ikindStr Static = "static"
ikindStr Interface = "interface"

getStr' :: DexFile -> StringId -> PP.Doc
getStr' _ sid | sid == -1 = "unknown"
getStr' dex sid =
  maybe ("<unknown string ID: " <> word32HexFixed sid <> ">") pSDI $
  getStr dex sid

getTypeName' :: DexFile -> TypeId -> PP.Doc
getTypeName' dex tid =
  maybe ("<unknown type ID: " <> word16HexFixed tid <> ">") pSDI $
  getTypeName dex tid

getTypeName'' :: DexFile -> TypeId -> PP.Doc
getTypeName'' dex tid =
  maybe msg (pSDI . tailSDI) $ getTypeName dex tid
    where tailSDI = BS.map slashToDot . BS.init . BS.tail
          slashToDot '/' = '.'
          slashToDot '$' = '.'
          slashToDot c = c
          msg = "<unknown type ID: " <> word16HexFixed tid <> ">"

fieldStr :: DexFile -> FieldId -> PP.Doc
fieldStr dex fid =
  case getField dex fid of
    Nothing -> "<unknown field ID: " <> word16HexFixed fid <> ">"
    Just fld ->
      getTypeName' dex (fieldClassId fld) <> "." <>
      getStr' dex (fieldNameId fld) <> ":" <>
      getTypeName' dex (fieldTypeId fld)

protoDesc' :: DexFile -> ProtoId -> PP.Doc
protoDesc' dex pid =
  case getProto dex pid of
    Nothing -> "<unknown prototype ID: " <> word16HexFixed pid <> ">"
    Just proto -> protoDesc dex proto

methodStr :: DexFile -> MethodId -> PP.Doc
methodStr dex mid =
  case getMethod dex mid of
    Nothing -> "<unknown method ID: " <> word16HexFixed mid <> ">"
    Just meth -> getTypeName'' dex (methClassId meth) <> "." <>
                 getStr' dex (methNameId meth) <> ":" <>
                 protoDesc' dex (methProtoId meth)

methodStr' :: DexFile -> MethodId -> PP.Doc
methodStr' dex mid =
  case getMethod dex mid of
    Nothing -> "<unknown method ID: " <> word16HexFixed mid <> ">"
    Just meth ->
      getTypeName' dex (methClassId meth) <> "." <>
      getStr' dex (methNameId meth) <> ":" <>
      protoDesc' dex (methProtoId meth)

protoDesc :: DexFile -> Proto -> PP.Doc
protoDesc dex proto =
  M.mconcat [ "(", argStr, ")", retStr ]
  where argStr = M.mconcat $ map (getTypeName' dex) (protoParams proto)
        retStr = getTypeName' dex (protoRet proto)

prettyField :: String -> PP.Doc -> PP.Doc
prettyField str v = padRight 20 str <> ":" <+> v

prettyField16 :: String -> Word16 -> PP.Doc
prettyField16 str (-1) = prettyField str "-1"
prettyField16 str v = prettyField str (word16Dec v)

prettyField32 :: String -> Word32 -> PP.Doc
prettyField32 str (-1) = prettyField str "-1"
prettyField32 str v = prettyField str (word32Dec v)

prettyFieldHex :: String -> Word32 -> PP.Doc
prettyFieldHex str v = prettyField str (M.mconcat [word32Dec v, "(0x ",  word24HexFixed v, ")"])

prettyFieldHex4 :: String -> Word32 -> PP.Doc
prettyFieldHex4 str v = prettyField str (M.mconcat [word32Dec v, " (0x", word16HexFixed (fromIntegral v), ")"])

prettyFieldHexS :: String -> Word32 -> PP.Doc -> PP.Doc
prettyFieldHexS n v s =
  prettyField n $ M.mconcat [ "0x", hp v, " (", s, ")" ]
  where
    hp = if v >= 0x10000
         then word20HexFixed
         else (word16HexFixed . fromIntegral)

escape :: String -> String
escape [] = []
escape ('\0' : s) = '\\' : '0' : escape s
escape ('\n' : s) = '\\' : 'n' : escape s
escape (c : s) = c : escape s

escapebs :: BS.ByteString -> BS.ByteString
escapebs = BS.pack . escape . BS.unpack

prettyHeader :: FilePath -> DexHeader -> PP.Doc
prettyHeader fp hdr =
  vsep [ "Opened" <+> PP.quotes (PP.text fp) <> ", DEX version " <> PP.quotes (pSDI (dexVersion hdr))
          , "DEX file header:"
          , prettyField "magic" (PP.quotes (pSDI (escapebs (BS.append (dexMagic hdr) (dexVersion hdr)))))
          , prettyField "checksum" (word32HexFixed (dexChecksum hdr))
          , prettyField "signature" sig
          , prettyField32 "file_size" (dexFileLen hdr)
          , prettyField32 "header_size" (dexHdrLen hdr)
          , prettyField32 "link_size" (dexLinkSize hdr)
          , prettyFieldHex "link_off" (dexLinkOff hdr)
          , prettyField32 "string_ids_size" (dexNumStrings hdr)
          , prettyFieldHex "string_ids_off" (dexOffStrings hdr)
          , prettyField32 "type_ids_size" (dexNumTypes hdr)
          , prettyFieldHex "type_ids_off" (dexOffTypes hdr)
          , prettyField32 "field_ids_size" (dexNumFields hdr)
          , prettyFieldHex "field_ids_off" (dexOffFields hdr)
          , prettyField32 "method_ids_size" (dexNumMethods hdr)
          , prettyFieldHex "method_ids_off" (dexOffMethods hdr)
          , prettyField32 "class_defs_size" (dexNumClassDefs hdr)
          , prettyFieldHex "class_defs_off" (dexOffClassDefs hdr)
          , prettyField32 "data_size" (dexDataSize hdr)
          , prettyFieldHex "data_off" (dexDataOff hdr)
          ]
  where
    sig = M.mconcat [ M.mconcat (take 2 sigStrings), "...", M.mconcat (drop 18 sigStrings) ]
    sigStrings = map word8HexFixed (dexSHA1 hdr)

prettyClassDef :: DexFile -> (TypeId, Class) -> PP.Doc
prettyClassDef dex (i, cls) =
  vsep [ PP.hcat [ "Class #", word16Dec i, " header:" ]
          , prettyField16 "class_idx" (classId cls)
          , prettyFieldHex4 "access_flags" (classAccessFlags cls)
          , prettyField16 "superclass_idx" (classSuperId cls)
          , prettyFieldHex "interfaces_off" (classInterfacesOff cls)
          , prettyField32 "source_file_idx" (classSourceNameId cls)
          , prettyFieldHex "annotations_off" (classAnnotsOff cls)
          , prettyFieldHex "class_data_off" (classDataOff cls)
          , prettyField32 "static_fields_size" (fromIntegral (length (classStaticFields cls)))
          , prettyField32 "instance_fields_size" (fromIntegral (length (classInstanceFields cls)))
          , prettyField32 "direct_methods_size" (fromIntegral (length (classDirectMethods cls)))
          , prettyField32 "virtual_methods_size" (fromIntegral (length (classVirtualMethods cls)))
          , ""
          , PP.hcat ["Class #", word16Dec i, "          -"]
          , prettyField "  Class descriptor" (PP.quotes (getTypeName' dex (classId cls)))
          , prettyFieldHexS "  Access flags" (classAccessFlags cls) (AF.flagsString AF.AClass (classAccessFlags cls))
          , prettyField "  Superclass" (PP.quotes (getTypeName' dex (classSuperId cls)))
          , "  Annotations      -"
          , PP.nest 4 $ vsep (map (prettyVisibleAnnotation dex) cannots)
          , "  Interfaces       -"
          , vsep (map (prettyInterface dex) (zip [0..] (classInterfaces cls)))
          , "  Static fields    -"
          , vsep (map (prettyEncodedField dex fannots) (zip [0..] (classStaticFields cls)))
          , "  Instance fields  -"
          , vsep (map (prettyEncodedField dex fannots) (zip [0..] (classInstanceFields cls)))
          , "  Direct methods   -"
          , vsep (map (prettyEncodedMethod dex mannots) (zip [0..] (classDirectMethods cls)))
          , "  Virtual methods  -"
          , vsep (map (prettyEncodedMethod dex mannots) (zip [0..] (classVirtualMethods cls)))
          ]
  where
    ClassAnnotations { classAnnotationsAnnot = cannots
                     , classFieldAnnotations = fannots
                     , classMethodAnnotations = mannots
                     } = classAnnotations cls


prettyAnnotationVisibility :: AnnotationVisibility -> PP.Doc
prettyAnnotationVisibility v =
  case v of
    AVBuild -> "BUILD"
    AVRuntime -> "RUNTIME"
    AVSystem -> "SYSTEM"

prettyVisibleAnnotation :: DexFile -> VisibleAnnotation -> PP.Doc
prettyVisibleAnnotation dex va =
  PP.hcat [ "<" <> prettyAnnotationVisibility (vaVisibility va) <> ">"
          , " "
          , prettyAnnotation dex (vaAnnotation va)
          ]

prettyAnnotation :: DexFile -> Annotation -> PP.Doc
prettyAnnotation dex a =
  getTypeName' dex (annotTypeId a) <> PP.parens (PP.hcat (PP.punctuate ", " (map prettyElement (annotElements a))))
  where
    prettyElement (enameIx, eval) = getStr' dex enameIx <> "=" <> prettyEncodedValue dex eval


prettyEncodedValue :: DexFile -> EncodedValue -> PP.Doc
prettyEncodedValue dex ev =
  case ev of
    EncodedByte i -> "(int8)" <> PP.int (fromIntegral i)
    EncodedShort i -> "(int16)" <> PP.int (fromIntegral i)
    EncodedChar c -> PP.quotes (PP.char c)
    EncodedInt i -> "(int32)" <> PP.int (fromIntegral i)
    EncodedLong i -> "(int64)" <> PP.integer (fromIntegral i)
    EncodedFloat f -> "(float)" <> PP.float f
    EncodedDouble d -> "(double)" <> PP.double d
    EncodedStringRef sid -> PP.doubleQuotes (getStr' dex sid)
    EncodedTypeRef tid -> getTypeName' dex tid
    EncodedFieldRef fid -> fieldStr dex fid
    EncodedMethodRef mid -> methodStr dex mid
    EncodedEnumRef fid -> fieldStr dex fid
    EncodedArray vs -> PP.brackets $ PP.hcat (PP.punctuate ", " (map (prettyEncodedValue dex) vs))
    EncodedAnnotation a -> prettyAnnotation dex a
    EncodedNull -> "(null)"
    EncodedBool b -> PP.text (show b)


prettyInterface :: DexFile -> (Word32, TypeId) -> PP.Doc
prettyInterface dex (n, iface) =
  M.mconcat [ "    #", PP.int (fromIntegral n), ": ", PP.quotes (getTypeName' dex iface) ]

prettyEncodedField :: DexFile -> [(FieldId, [VisibleAnnotation])] -> (Word32, EncodedField) -> PP.Doc
prettyEncodedField dex _annots (n, f) =
  case getField dex (fieldId f) of
    Nothing -> "<unknown field ID: " <> word16HexFixed (fieldId f) <> ">"
    Just fld ->
      vsep [ M.mconcat ["    #", word32Dec n, "              : (in ", getTypeName' dex (fieldClassId fld), ")"]
              , prettyField "      name" (PP.quotes (getStr' dex (fieldNameId fld)))
              , prettyField "      type" (PP.quotes (getTypeName' dex (fieldTypeId fld)))
              , prettyFieldHexS "      access" (fieldAccessFlags f) (AF.flagsString AF.AField (fieldAccessFlags f))
              ]

prettyEncodedMethod :: DexFile -> [(MethodId, [VisibleAnnotation])] -> (Word32, EncodedMethod) -> PP.Doc
prettyEncodedMethod dex annots (n, m) =
  case (mmeth, mmeth >>= (getProto dex . methProtoId)) of
    (Just method, Just proto) ->
      let flags = methAccessFlags m
          adoc | Just vannots <- lookup (methId m) annots =
                   "      annots" $+$ PP.nest 8 (vsep (map (prettyVisibleAnnotation dex) vannots))
               | otherwise = PP.empty
      in vsep [ "    #" <> word32Dec n <> "              : (in" <+> getTypeName' dex (methClassId method) <> ")"
                 , prettyField "      name" (PP.quotes (getStr' dex (methNameId method)))
                 , prettyField "      type" (PP.quotes (protoDesc dex proto))
                 , prettyFieldHexS "      access" flags (AF.flagsString AF.AMethod flags)
                 , adoc
                 , maybe ("     code:    (none)") (prettyCode dex flags (methId m)) (methCode m)
                 ]
    (Nothing, _) -> "<unknown method ID: " <> word16HexFixed (methId m) <> ">"
    (Just method, Nothing) -> "<unknown prototype ID: " <> word16HexFixed (methProtoId method) <> ">"
  where
    mmeth = getMethod dex (methId m)

-- | Like 'PP.vcat', except it never collapses overlapping lines
vsep :: [PP.Doc] -> PP.Doc
vsep = F.foldl' ($+$) M.mempty

prettyCode :: DexFile -> AF.AccessFlags -> MethodId -> CodeItem -> PP.Doc
prettyCode dex flags mid codeItem =
  vsep [ "      code       -"
          , prettyField16 "      registers" (codeRegs codeItem)
          , prettyField16 "      ins" (codeInSize codeItem)
          , prettyField16 "      outs" (codeOutSize codeItem)
          , prettyField "      insns size" (word32Dec (fromIntegral (length insnUnits)) <+> "16-bit code units")
          , word24HexFixed nameAddr <> ":                                        |[" <> word24HexFixed nameAddr <> "] " <> methodStr dex mid
          , insnText
          , prettyField "      catches" (if null tries then "(none)" else word32Dec (fromIntegral (length tries)))
          , vsep (map (prettyTryBlock dex codeItem) tries)
          , prettyField "      positions" ""
          , vsep (map prettyPosition (reverse (dbgPositions debugState)))
          , prettyField "      locals" ""
          , vsep (map (prettyLocal dex) locals)
          ]
  where
    tries = codeTryItems codeItem
    insnUnits = codeInsns codeItem
    debugState = either (const DI.emptyDebugState) id (DI.executeDebugInsns dex codeItem flags mid)
    addr = codeInsnOff codeItem
    nameAddr = addr - 16
    decodeErrorMsg = maybe "Unknown error" decodeErrorAsString . E.fromException
    insnText = either (\msg -> "error parsing instructions: " <> PP.text (decodeErrorMsg msg))
                      (formatInstructions dex addr 0 insnUnits)
                      (decodeInstructions insnUnits)
    locals = L.sortBy cmpLocal [ (r, l) | (r, ls) <- Map.toList (dbgLocals debugState)
                                        , l <- ls
                                        , hasLocal l
                                        ]
    cmpLocal (_, LocalInfo n _ e _ _ _) (_, LocalInfo n' _ e' _ _ _) =
      compare (e, n) (e', n')
    hasLocal (LocalInfo _ _ _ nid _ _) = nid /= (-1)

prettyLocal :: DexFile -> (Word32, LocalInfo) -> PP.Doc
prettyLocal dex (r, LocalInfo _ s e nid tid sid) =
  M.mconcat [ "        0x", word16HexFixed (fromIntegral s), " - 0x", word16HexFixed (fromIntegral e)
          , " reg=", word32Dec r, " ", getStr' dex (fromIntegral nid), " "
          , getTypeName' dex (fromIntegral tid), " ", if sid == -1 then "" else getStr' dex (fromIntegral sid)
          ]

prettyPosition :: PositionInfo -> PP.Doc
prettyPosition (PositionInfo a l) = "        0x" <> word16HexFixed (fromIntegral a) <> " line=" <> word32Dec l

prettyTryBlock :: DexFile -> CodeItem -> TryItem -> PP.Doc
prettyTryBlock dex codeItem tryItem =
  vsep [ M.mconcat [ "        0x", word16HexFixed (fromIntegral (tryStartAddr tryItem)), " - 0x", word16HexFixed (fromIntegral end) ]
          , vsep [ "        " <> getTypeName' dex ty <> " -> 0x" <> word16HexFixed (fromIntegral addr)
                    | (ty, addr) <- handlers
                    ]
          , vsep [ "        <any> -> 0x" <> word16HexFixed (fromIntegral addr)
                    | addr <- mapMaybe chAllAddr catches
                    ]
          ]
  where
    end = tryStartAddr tryItem + fromIntegral (tryInsnCount tryItem)
    catches = filter ((== tryHandlerOff tryItem) . fromIntegral . chHandlerOff) (codeHandlers codeItem)
    handlers = M.mconcat (map chHandlers catches)

formatInstructions :: DexFile -> Word32 -> Word32 -> [Word16] -> [Instruction] -> PP.Doc
formatInstructions _ _ _ [] [] = M.mempty
formatInstructions _ _ _ [] is =
  "ERROR: No more code units (" <> PP.text (show is) <> " instructions left)"
formatInstructions _ _ _ ws [] =
  "ERROR: No more instructions (" <> PP.int (length ws) <> " code units left)"
formatInstructions dex addr off ws (i:is) =
  M.mconcat [ word24HexFixed addr
          , ": "
          , unitDoc
          , "|"
          , word16HexFixed (fromIntegral off)
          , ": "
          , idoc
          ] $+$ formatInstructions dex (addr + (l' * 2)) (off + l') ws' is
  where
    (iws, ws') = L.splitAt l ws
    istrs = [ word8HexFixed (fromIntegral (w .&. 0x00FF)) <>
              word8HexFixed (fromIntegral ((w .&. 0xFF00) `shiftR` 8))
            | w <- iws
            ]
    istrs' | length istrs < 8 = take 8 (istrs ++ repeat "    ")
           | otherwise = take 7 istrs ++ ["... "]
    l = insnUnitCount i
    l' = fromIntegral l
    unitDoc = M.mconcat (PP.punctuate " " istrs')
    idoc = prettyInstruction dex off i

prettyInstruction :: DexFile -> Word32 -> Instruction -> PP.Doc
prettyInstruction _ _ Nop = "nop" <> " // spacer"
prettyInstruction _ _ (Move mty dst src) =
  M.mconcat
  [ moveTypeString mty
  , moveSizeString dst, " "
  , regStr dst, ", ", regStr src
  ]
prettyInstruction _ _ (Move1 MResult r) = "move-result " <> regStr r
prettyInstruction _ _ (Move1 MResultWide r) = "move-result-wide " <> regStr r
prettyInstruction _ _ (Move1 MResultObject r) = "move-result-object " <> regStr r
prettyInstruction _ _ (Move1 MException r) = "move-exception " <> regStr r
prettyInstruction _ _ ReturnVoid = "return-void"
prettyInstruction _ _ (Return MNormal r) = "return " <> regStr r
prettyInstruction _ _ (Return MWide r) = "return-wide " <> regStr r
prettyInstruction _ _ (Return MObject r) = "return-object " <> regStr r
prettyInstruction dex _ (LoadConst d c@(Const4 _)) =
  constStr dex "const/4" d c
prettyInstruction dex _ (LoadConst d c@(Const16 _)) =
  constStr dex "const/16" d c
prettyInstruction dex _ (LoadConst d c@(ConstHigh16 _)) =
  constStr dex "const/high16" d c
prettyInstruction dex _ (LoadConst d c@(ConstWide16 _)) =
  constStr dex "const-wide/16" d c
prettyInstruction dex _ (LoadConst d c@(Const32 _)) =
  constStr dex "const" d c
prettyInstruction dex _ (LoadConst d c@(ConstWide32 _)) =
  constStr dex "const-wide/32" d c
prettyInstruction dex _ (LoadConst d c@(ConstWide _)) =
  constStr dex "const-wide" d c
prettyInstruction dex _ (LoadConst d c@(ConstWideHigh16 _)) =
  constStr dex "const-wide/high16" d c
prettyInstruction dex _ (LoadConst d c@(ConstString _)) =
  constStr dex "const-string" d c
prettyInstruction dex _ (LoadConst d c@(ConstStringJumbo _)) =
  constStr dex "const-string/jumbo" d c
prettyInstruction dex _ (LoadConst d c@(ConstClass _)) =
  constStr dex "const-class" d c
prettyInstruction _ _ (MonitorEnter r) = "monitor-enter v" <> word8Dec r
prettyInstruction _ _ (MonitorExit r) = "monitor-exit v" <> word8Dec r
prettyInstruction dex _ (CheckCast r tid) =
  M.mconcat ["check-cast v", word8Dec r,  ", ", getTypeName' dex tid] <>
  typeComm tid
prettyInstruction dex _ (InstanceOf dst ref tid) =
  mkInsn "instance-of"
         [ iregStr8 dst, iregStr8 ref, getTypeName' dex tid ] <>
  typeComm tid
prettyInstruction _ _ (ArrayLength dst ref) =
  mkInsn'8 "array-length" [ dst, ref ]
prettyInstruction dex _ (NewInstance dst tid) =
  mkInsn "new-instance" [ iregStr8 dst, getTypeName' dex tid ] <>
  typeComm tid
prettyInstruction dex _ (NewArray dst sz tid) =
  mkInsn "new-array"
         [ iregStr8 dst, iregStr8 sz, getTypeName' dex tid ] <>
  typeComm tid
prettyInstruction dex _ (FilledNewArray tid rs) =
  mkInsnb "filled-new-array"
          (map iregStr8 rs) [ getTypeName' dex tid ] <>
  typeComm tid
prettyInstruction dex _ (FilledNewArrayRange tid rs) =
  mkInsnb "filled-new-array/range"
          (map iregStr16 rs) [ getTypeName' dex tid ] <>
  typeComm tid
prettyInstruction _ a (FillArrayData dst off) =
  mkInsn "fill-array-data"
         [ iregStr8 dst, word32HexFixed (a + fromIntegral off) ] <>
  offComm' off
prettyInstruction _ _ (Throw r) = "throw v" <> word8Dec r
prettyInstruction _ a (Goto off) =
  "goto " <> word16HexFixed (fromIntegral (a + fromIntegral off)) <>
  offComm (fromIntegral off :: Int16)
prettyInstruction _ a (Goto16 off) =
  "goto/16 " <> word16HexFixed (fromIntegral (a + fromIntegral off)) <>
  offComm off
prettyInstruction _ a (Goto32 off) =
  "goto/32 " <> word32HexFixed (fromIntegral (a + fromIntegral off)) <>
  offComm32 off
prettyInstruction _ a (PackedSwitch r off) =
  mkInsn "packed-switch"
         [ iregStr8 r, word32HexFixed (a + fromIntegral off) ] <>
  offComm' off
prettyInstruction _ a (SparseSwitch r off) =
  mkInsn "sparse-switch"
         [ iregStr8 r, word32HexFixed (a + fromIntegral off) ] <>
  offComm' off
prettyInstruction _ _ (Cmp CLFloat dst r1 r2) =
  mkInsn'8 "cmpl-float" [dst, r1, r2]
prettyInstruction _ _ (Cmp CGFloat dst r1 r2) =
  mkInsn'8 "cmpg-float" [dst, r1, r2]
prettyInstruction _ _ (Cmp CLDouble dst r1 r2) =
  mkInsn'8 "cmpl-double" [dst, r1, r2]
prettyInstruction _ _ (Cmp CGDouble dst r1 r2) =
  mkInsn'8 "cmpg-double" [dst, r1, r2]
prettyInstruction _ _ (Cmp CLong dst r1 r2) =
  mkInsn'8 "cmp-long" [dst, r1, r2]
prettyInstruction _ a (If op r1 r2 off) =
  mkInsn ("if-" <> ifOpStr op)
         [ iregStr8 r1, iregStr8 r2
         , word16HexFixed (fromIntegral (a + fromIntegral off))
         ]
  <> offComm off
prettyInstruction _ a (IfZero op r off) =
  mkInsn ("if-" <> ifOpStr op <> "z")
         [ iregStr8 r, word16HexFixed (fromIntegral (a + fromIntegral off)) ]
  <> offComm off
prettyInstruction _ _ (ArrayOp op val arr idx) =
  mkInsn'8 ("a" <> accessOpStr op) [ val, arr, idx ]
prettyInstruction dex _ (InstanceFieldOp op val obj fid) =
  mkInsn ("i" <> accessOpStr op)
         [ iregStr8 val, iregStr8 obj, fieldStr dex fid ]
  <> fieldComm fid
prettyInstruction dex _ (StaticFieldOp op val fid) =
  mkInsn ("s" <> accessOpStr op) [ iregStr8 val, fieldStr dex fid ]
  <> fieldComm fid
prettyInstruction dex _ (Invoke kind range mid args) =
  mkInsn ("invoke-" <>
          ikindStr kind <>
          if range then "/range" else "")
         [ PP.brackets (M.mconcat (PP.punctuate ", " (map iregStr16 args)))
         , methodStr' dex mid
         ] <>
  methodComm mid
prettyInstruction _ _ (Unop op r1 r2) =
  mkInsn'8 (unopStr op) [r1, r2]
prettyInstruction _ _ (IBinop op False dst r1 r2) =
  mkInsn'8 (binopStr op <> "-int") [ dst, r1, r2 ]
prettyInstruction _ _ (IBinop op True dst r1 r2) =
  mkInsn'8 (binopStr op <> "-long") [ dst, r1, r2 ]
prettyInstruction _ _ (FBinop op False dst r1 r2) =
  mkInsn'8 (binopStr op <> "-float") [ dst, r1, r2 ]
prettyInstruction _ _ (FBinop op True dst r1 r2) =
  mkInsn'8 (binopStr op <> "-double") [ dst, r1, r2 ]
prettyInstruction _ _ (IBinopAssign op False dst src) =
  mkInsn'8 (binopStr op <> "-int/2addr") [ dst, src ]
prettyInstruction _ _ (IBinopAssign op True dst src) =
  mkInsn'8 (binopStr op <> "-long/2addr") [ dst, src ]
prettyInstruction _ _ (FBinopAssign op False dst src) =
  mkInsn'8 (binopStr op <> "-float/2addr") [ dst, src ]
prettyInstruction _ _ (FBinopAssign op True dst src) =
  mkInsn'8 (binopStr op <> "-double/2addr") [ dst, src ]
prettyInstruction _ _ (BinopLit16 RSub dst src i) =
  mkInsn "rsub-int"
         [iregStr8 dst, iregStr8 src, "#int " <> int16Dec i] <>
  intComm16' (fromIntegral i)
prettyInstruction _ _ (BinopLit16 op dst src i) =
  mkInsn (binopStr op <> "-int/lit16")
         [iregStr8 dst, iregStr8 src, "#int " <> int16Dec i] <>
  intComm16' (fromIntegral i)
prettyInstruction _ _ (BinopLit8 op dst src i) =
  mkInsn (binopStr op <> "-int/lit8")
         [iregStr8 dst, iregStr8 src, "#int " <> int8Dec i] <>
  intComm8' (fromIntegral i)
prettyInstruction _ _ i@(PackedSwitchData{}) =
  "packed-switch-data (" <> int32Dec (fromIntegral size) <> " units)"
    where size = insnUnitCount i
prettyInstruction _ _ i@(SparseSwitchData{}) =
  "sparse-switch-data (" <> int32Dec (fromIntegral size) <> " units)"
    where size = insnUnitCount i
prettyInstruction _ _ i@(ArrayData{}) =
  "array-data (" <> int32Dec (fromIntegral size) <> " units)"
    where size = insnUnitCount i

-- Helpers


-- | Right pad a string with spaces until it is the specified width
padRight :: Int -> String -> PP.Doc
padRight n s = PP.text (s ++ replicate (n - length s) ' ')

-- | Lift a bytestring into a 'Doc' unsafely
pSDI :: BS.ByteString -> PP.Doc
pSDI = PP.text . BS.unpack

mkInsn :: PP.Doc -> [PP.Doc] -> PP.Doc
mkInsn name args = name <+> M.mconcat (PP.punctuate ", " args)
{-# INLINE mkInsn #-}

mkInsn'8 :: PP.Doc -> [Word8] -> PP.Doc
mkInsn'8 name args = mkInsn name (map iregStr8 args)
{-# INLINE mkInsn'8 #-}

mkInsnb :: PP.Doc -> [PP.Doc] -> [PP.Doc] -> PP.Doc
mkInsnb name bargs args =
  name <+> PP.braces (M.mconcat (PP.punctuate ", " bargs)) <> "," <+> M.mconcat (PP.punctuate ", " args)
{-# INLINE mkInsnb #-}

word16Hex :: Word16 -> PP.Doc
word16Hex w = PP.text (printf "%x" w)

word16HexFixed :: Word16 -> PP.Doc
word16HexFixed w = PP.text (printf "%0.4x" w)

word8Hex :: Word8 -> PP.Doc
word8Hex w = PP.text (printf "%x" w)

word8HexFixed :: Word8 -> PP.Doc
word8HexFixed w = PP.text (printf "%0.2x" w)

word24HexFixed :: Word32 -> PP.Doc
word24HexFixed w = M.mconcat [ word8HexFixed (fromIntegral (w .&. 0x00FF0000) `shiftR` 16)
                           , word16HexFixed (fromIntegral (w .&. 0x0000FFFF))
                           ]

word20HexFixed :: Word32 -> PP.Doc
word20HexFixed w = M.mconcat [ word8Hex (fromIntegral (w .&. 0x00FF0000) `shiftR` 16)
                           , word16HexFixed (fromIntegral (w .&. 0x0000FFFF))
                           ]

word32HexFixed :: Word32 -> PP.Doc
word32HexFixed w = PP.text (printf "%0.8x" w)

word64HexFixed :: Word64 -> PP.Doc
word64HexFixed w = PP.text (printf "%0.16x" w)

int16HexFixed :: Int16 -> PP.Doc
int16HexFixed w = PP.text (printf "%0.4x" w)

int32HexFixed :: Int32 -> PP.Doc
int32HexFixed w = PP.text (printf "%0.8x" w)

word8Dec :: Word8 -> PP.Doc
word8Dec w = PP.text (printf "%d" w)

word16Dec :: Word16 -> PP.Doc
word16Dec w = PP.text (printf "%d" w)

word32Dec :: Word32 -> PP.Doc
word32Dec w = PP.text (printf "%d" w)

int8Dec :: Int8 -> PP.Doc
int8Dec w = PP.text (printf "%d" w)

int16Dec :: Int16 -> PP.Doc
int16Dec w = PP.text (printf "%d" w)

int32Dec :: Int32 -> PP.Doc
int32Dec w = PP.text (printf "%d" w)

int64Dec :: Int64 -> PP.Doc
int64Dec w = PP.text (printf "%d" w)
