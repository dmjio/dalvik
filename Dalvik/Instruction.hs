module Dalvik.Instruction
  ( decodeInstructions
  , insnUnitCount
  , Reg(..)
  , ConstArg(..)
  , MoveType(..)
  , Move1Type(..)
  , AccessType(..)
  , AccessOp(..)
  , InvokeKind(..)
  , CType(..)
  , CmpOp(..)
  , IfOp(..)
  , Unop(..)
  , Binop(..)
  , Instruction(..)
  , DecodeError
  ) where

import Control.Applicative()
import Control.Monad
import Data.Array
import Data.Bits
import Data.Function
import Data.Int
import Data.Word
import Text.Printf

import Dalvik.Types

data Reg
  = R4 Reg4
  | R8 Reg8
  | R16 Reg16
    deriving (Eq, Ord, Show)

data ConstArg
  = Const4 Int32
  | Const16 Int32
  | Const32 Int32
  | ConstHigh16 Int32
  | ConstWide16 Int64
  | ConstWide32 Int64
  | ConstWide Int64
  | ConstWideHigh16 Int64
  | ConstString StringId
  | ConstStringJumbo StringId
  | ConstClass TypeId
    deriving (Eq, Ord, Show)

-- TODO: what's the best encoding for move instructions?

data MoveType
  = MNormal
  | MWide
  | MObject
    deriving (Eq, Ord, Show)

data Move1Type
  = MResult
  | MResultWide
  | MResultObject
  | MException
    deriving (Eq, Ord, Show)

data AccessType
  = AWide
  | AObject
  | ABoolean
  | AByte
  | AChar
  | AShort
    deriving (Eq, Ord, Show)

data AccessOp
  = Get (Maybe AccessType)
  | Put (Maybe AccessType)
    deriving (Eq, Ord, Show)

data InvokeKind
  = Virtual
  | Super
  | Direct
  | Static
  | Interface
    deriving (Eq, Ord, Show)

data CType
  = Byte
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
    deriving (Eq, Ord, Show)

data CmpOp
  = CLFloat
  | CGFloat
  | CLDouble
  | CGDouble
  | CLong
    deriving (Eq, Ord, Show)

data IfOp
  = Eq
  | Ne
  | Lt
  | Ge
  | Gt
  | Le
    deriving (Eq, Ord, Show)

data Unop
  = NegInt
  | NotInt
  | NegLong
  | NotLong
  | NegFloat
  | NegDouble
  | Convert CType CType
    deriving (Eq, Ord, Show)

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  | And
  | Or
  | Xor
  | Shl
  | Shr
  | UShr
  | RSub
    deriving (Eq, Ord, Show)

data Instruction
  = Nop
  | Move MoveType Reg Reg
  | Move1 Move1Type Reg
  | ReturnVoid
  | Return MoveType Reg
  | LoadConst Reg ConstArg
  | MonitorEnter Reg8
  | MonitorExit Reg8
  | CheckCast Reg8 TypeId
  | InstanceOf Reg4 Reg4 TypeId
  | ArrayLength Reg4 Reg4
  | NewInstance Reg8 TypeId
  | NewArray Reg4 Reg4 TypeId
  -- TODO: is this a good encoding for array instructions?
  | FilledNewArray TypeId [Reg4]
  | FilledNewArrayRange TypeId [Reg16]
  | FillArrayData Reg8 Word32 -- [Word8]
  | Throw Reg8
  | Goto Int8
  | Goto16 Int16
  | Goto32 Int32
  | PackedSwitch Reg8 Word32
  | SparseSwitch Reg8 Word32
  | Cmp CmpOp Reg8 Reg8 Reg8
  | If IfOp Reg4 Reg4 Int16
  | IfZero IfOp Reg8 Int16
  | ArrayOp AccessOp Reg8 Reg8 Reg8
  | InstanceFieldOp AccessOp Reg4 Reg4 FieldId
  | StaticFieldOp AccessOp Reg8 FieldId
  -- TODO: how best to encode invoke instructions?
  | Invoke InvokeKind Bool MethodId [Reg16]
  | Unop Unop Reg4 Reg4
  | IBinop Binop Bool Reg8 Reg8 Reg8
  | FBinop Binop Bool Reg8 Reg8 Reg8
  | IBinopAssign Binop Bool Reg4 Reg4
  | FBinopAssign Binop Bool Reg4 Reg4
  | BinopLit16 Binop Reg4 Reg4 Int16
  | BinopLit8 Binop Reg8 Reg8 Int8
  | PackedSwitchData Int32 [Word16]      -- TODO: Int32
  | SparseSwitchData [Word16] [Word16]   -- TODO: Int32
  | ArrayData Word16 Word32 [Word16] -- TODO
    deriving (Eq, Ord, Show)

insnUnitCount :: Instruction -> Int
insnUnitCount i =
  case i of
    Nop -> 1
    Move _ (R4 _) _ -> 1
    Move _ (R8 _) _ -> 2
    Move _ (R16 _) _ -> 3
    Move1 {} -> 1
    ReturnVoid -> 1
    Return {} -> 1
    LoadConst _ (Const4 _) -> 1
    LoadConst _ (Const16 _) -> 2
    LoadConst _ (ConstHigh16 _) -> 2
    LoadConst _ (ConstWide16 _) -> 2
    LoadConst _ (Const32 _) -> 3
    LoadConst _ (ConstWide32 _) -> 3
    LoadConst _ (ConstWide _) -> 5
    LoadConst _ (ConstWideHigh16 _) -> 2
    LoadConst _ (ConstString _) -> 2
    LoadConst _ (ConstStringJumbo _) -> 3
    LoadConst _ (ConstClass _) -> 2
    MonitorEnter {} -> 1
    MonitorExit {} -> 1
    CheckCast {} -> 2
    InstanceOf {} -> 2
    ArrayLength {} -> 1
    NewInstance {} -> 2
    NewArray {} -> 2
    FilledNewArray {} -> 3
    FilledNewArrayRange {} -> 3
    FillArrayData {} -> 3
    Throw {} -> 1
    Goto {} -> 1
    Goto16 {} -> 2
    Goto32 {} -> 3
    PackedSwitch {} -> 3
    SparseSwitch {} -> 3
    Cmp {} -> 2
    If {} -> 2
    IfZero {} -> 2
    ArrayOp {} -> 2
    InstanceFieldOp {} -> 2
    StaticFieldOp {} -> 2
    Invoke {} -> 3
    Unop {} -> 1
    IBinop {} -> 2
    FBinop {} -> 2
    IBinopAssign {} -> 1
    FBinopAssign {} -> 1
    BinopLit16 {} -> 2
    BinopLit8 {} -> 2
    PackedSwitchData _ ts -> length ts + 4
    SparseSwitchData _ ts -> (length ts * 2) + 2
    ArrayData _ _ vs -> length vs + 4

splitWord8 :: Word8 -> (Word4, Word4)
splitWord8 w = (fromIntegral $ w `shiftR` 4, fromIntegral $ w .&. 0x0F)

splitWord16 :: Word16 -> (Word8, Word8)
splitWord16 w = (fromIntegral $ w `shiftR` 8, fromIntegral $ w .&. 0x00FF)

splitWord16' :: Word16 -> (Word4, Word4, Word4, Word4)
splitWord16' w = (fst4 b1, snd4 b1, fst4 b2, snd4 b2)
  where (b1, b2) = splitWord16 w

type DecodeError = String -- TODO: replace with structured type

prematureEnd :: Word8 -> Word16 -> DecodeError
prematureEnd =
  printf "Premature end of data stream at opcode %02x (%04x)"

invalidOp :: Word8 -> DecodeError
invalidOp op = "Invalid opcode: " ++ show op

{- As named in the Dalvik VM Instruction Formats document. -}
data IFormatParser
  = IF10x Instruction
  | IF12x (Word4 -> Word4 -> Instruction)
  | IF11n (Word4 -> Word4 -> Instruction)
  | IF11x (Word8 -> Instruction)
  | IF10t (Word8 -> Instruction)
  | IF20t (Word16 -> Instruction)
  | IF22x (Word8 -> Word16 -> Instruction)
  | IF21t (Word8 -> Word16 -> Instruction)
  | IF21s (Word8 -> Word16 -> Instruction)
  | IF21h (Word8 -> Word16 -> Instruction)
  | IF21c (Word8 -> Word16 -> Instruction)
  | IF23x (Word8 -> Word8 -> Word8 -> Instruction)
  | IF22b (Word8 -> Word8 -> Word8 -> Instruction)
  | IF22t (Word4 -> Word4 -> Word16 -> Instruction)
  | IF22s (Word4 -> Word4 -> Word16 -> Instruction)
  | IF22c (Word4 -> Word4 -> Word16 -> Instruction)
  | IF22cs (Word4 -> Word4 -> Word16 -> Instruction)
  | IF30t (Word32 -> Instruction)
  | IF32x (Word16 -> Word16 -> Instruction)
  | IF31i (Word8 -> Word32 -> Instruction)
  | IF31t (Word8 -> Word32 -> Instruction)
  | IF31c (Word8 -> Word32 -> Instruction)
  | IF35c (Word16 -> [Word4] -> Instruction)
  | IF3rc (Word16 -> [Word16] -> Instruction)
  | IF51l (Word8 -> Word64 -> Instruction)
  | InvalidOp

iparseTable :: Array Word8 IFormatParser
iparseTable = array (0x00, 0xFF)
  [ (0x00, IF10x Nop)
  -- Move
  , (0x01, IF12x (Move MNormal `on` R4))
  , (0x02, IF22x $ \r1 r2 -> Move MNormal (R8 r1) (R16 r2))
  , (0x03, IF32x (Move MNormal `on` R16))
  , (0x04, IF12x (Move MWide `on` R4))
  , (0x05, IF22x $ \r1 r2 -> Move MWide (R8 r1) (R16 r2))
  , (0x06, IF32x (Move MWide `on` R16))
  , (0x07, IF12x (Move MObject `on` R4))
  , (0x08, IF22x $ \r1 r2 -> Move MObject (R8 r1) (R16 r2))
  , (0x09, IF32x (Move MObject `on` R16))
  , (0x0a, move1 MResult)
  , (0x0b, move1 MResultWide)
  , (0x0c, move1 MResultObject)
  , (0x0d, move1 MException)
  -- Return
  , (0x0e, IF10x ReturnVoid)
  , (0x0f, ret MNormal)
  , (0x10, ret MWide)
  , (0x11, ret MObject)
  -- Constants
  , (0x12, IF11n $ \r c -> LoadConst (R4 r) (const4 c))
  , (0x13, IF21s $ \r c -> LoadConst (R8 r) (const16 c))
  , (0x14, IF31i $ \r c -> LoadConst (R8 r) (const32 c))
  , (0x15, IF21h $ \r c -> LoadConst (R8 r) (constHigh16 c))
  , (0x16, IF21s $ \r c -> LoadConst (R8 r) (constWide16 c))
  , (0x17, IF31i $ \r c -> LoadConst (R8 r) (constWide32 c))
  , (0x18, IF51l $ \r c -> LoadConst (R8 r) (constWide c))
  , (0x19, IF21h $ \r c -> LoadConst (R8 r) (constWideHigh16 c))
  , (0x1a, IF21c $ \r i -> LoadConst (R8 r) (constString i))
  , (0x1b, IF31c $ \r i -> LoadConst (R8 r) (ConstStringJumbo i))
  , (0x1c, IF21c $ \r i -> LoadConst (R8 r) (ConstClass i))
  -- Monitors
  , (0x1d, IF11x MonitorEnter)
  , (0x1e, IF11x MonitorExit)
  -- Casting
  , (0x1f, IF21c CheckCast)
  , (0x20, IF22c InstanceOf)
  -- Arrays
  , (0x21, IF12x ArrayLength)
  , (0x22, IF21c NewInstance)
  , (0x23, IF22c NewArray)
  , (0x24, IF35c FilledNewArray)
  , (0x25, IF3rc FilledNewArrayRange)
  , (0x26, IF31t FillArrayData)
  -- Exceptions
  , (0x27, IF11x Throw)
  -- Unconditional branches
  , (0x28, IF10t $ Goto . fromIntegral)
  , (0x29, IF20t $ Goto16 . fromIntegral)
  , (0x2a, IF30t $ Goto32 . fromIntegral)
  -- Switch
  , (0x2b, IF31t $ \r o -> PackedSwitch r o)
  , (0x2c, IF31t $ \r o -> SparseSwitch r o)
  -- Comparisons
  , (0x2d, cmp CLFloat)
  , (0x2e, cmp CGFloat)
  , (0x2f, cmp CLDouble)
  , (0x30, cmp CGDouble)
  , (0x31, cmp CLong)
  -- If comparisons
  , (0x32, ifop Eq)
  , (0x33, ifop Ne)
  , (0x34, ifop Lt)
  , (0x35, ifop Ge)
  , (0x36, ifop Gt)
  , (0x37, ifop Le)
  -- If comparisons with zero
  , (0x38, ifzop Eq)
  , (0x39, ifzop Ne)
  , (0x3a, ifzop Lt)
  , (0x3b, ifzop Ge)
  , (0x3c, ifzop Gt)
  , (0x3d, ifzop Le)
  -- Unused
  , (0x3e, InvalidOp)
  , (0x3f, InvalidOp)
  , (0x40, InvalidOp)
  , (0x41, InvalidOp)
  , (0x42, InvalidOp)
  , (0x43, InvalidOp)
  -- Array operations
  , (0x44, arrop (Get Nothing))
  , (0x45, arrop (Get (Just AWide)))
  , (0x46, arrop (Get (Just AObject)))
  , (0x47, arrop (Get (Just ABoolean)))
  , (0x48, arrop (Get (Just AByte)))
  , (0x49, arrop (Get (Just AChar)))
  , (0x4a, arrop (Get (Just AShort)))
  , (0x4b, arrop (Put Nothing))
  , (0x4c, arrop (Put (Just AWide)))
  , (0x4d, arrop (Put (Just AObject)))
  , (0x4e, arrop (Put (Just ABoolean)))
  , (0x4f, arrop (Put (Just AByte)))
  , (0x50, arrop (Put (Just AChar)))
  , (0x51, arrop (Put (Just AShort)))
  -- Instance field operations
  , (0x52, instop (Get Nothing))
  , (0x53, instop (Get (Just AWide)))
  , (0x54, instop (Get (Just AObject)))
  , (0x55, instop (Get (Just ABoolean)))
  , (0x56, instop (Get (Just AByte)))
  , (0x57, instop (Get (Just AChar)))
  , (0x58, instop (Get (Just AShort)))
  , (0x59, instop (Put Nothing))
  , (0x5a, instop (Put (Just AWide)))
  , (0x5b, instop (Put (Just AObject)))
  , (0x5c, instop (Put (Just ABoolean)))
  , (0x5d, instop (Put (Just AByte)))
  , (0x5e, instop (Put (Just AChar)))
  , (0x5f, instop (Put (Just AShort)))
  -- Static field operations
  , (0x60, statop (Get Nothing))
  , (0x61, statop (Get (Just AWide)))
  , (0x62, statop (Get (Just AObject)))
  , (0x63, statop (Get (Just ABoolean)))
  , (0x64, statop (Get (Just AByte)))
  , (0x65, statop (Get (Just AChar)))
  , (0x66, statop (Get (Just AShort)))
  , (0x67, statop (Put Nothing))
  , (0x68, statop (Put (Just AWide)))
  , (0x69, statop (Put (Just AObject)))
  , (0x6a, statop (Put (Just ABoolean)))
  , (0x6b, statop (Put (Just AByte)))
  , (0x6c, statop (Put (Just AChar)))
  , (0x6d, statop (Put (Just AShort)))
  -- Invoke with fixed argument counts
  , (0x6e, invoke Virtual)
  , (0x6f, invoke Super)
  , (0x70, invoke Direct)
  , (0x71, invoke Static)
  , (0x72, invoke Interface)
  -- Unused
  , (0x73, InvalidOp)
  -- Invoke with arbitrary argument counts
  , (0x74, invokeRange Virtual)
  , (0x75, invokeRange Super)
  , (0x76, invokeRange Direct)
  , (0x77, invokeRange Static)
  , (0x78, invokeRange Interface)
  -- Unused
  , (0x79, InvalidOp)
  , (0x7a, InvalidOp)
  -- Unary operators
  , (0x7b, unop NegInt)
  , (0x7c, unop NotInt)
  , (0x7d, unop NegLong)
  , (0x7e, unop NotLong)
  , (0x7f, unop NegFloat)
  , (0x80, unop NegDouble)
  , (0x81, unop (Convert Int Long))
  , (0x82, unop (Convert Int Float))
  , (0x83, unop (Convert Int Double))
  , (0x84, unop (Convert Long Int))
  , (0x85, unop (Convert Long Float))
  , (0x86, unop (Convert Long Double))
  , (0x87, unop (Convert Float Int))
  , (0x88, unop (Convert Float Long))
  , (0x89, unop (Convert Float Double))
  , (0x8a, unop (Convert Double Int))
  , (0x8b, unop (Convert Double Long))
  , (0x8c, unop (Convert Double Float))
  , (0x8d, unop (Convert Int Byte))
  , (0x8e, unop (Convert Int Char))
  , (0x8f, unop (Convert Int Short))
  -- Binary operators
  , (0x90, ibinop Add)
  , (0x91, ibinop Sub)
  , (0x92, ibinop Mul)
  , (0x93, ibinop Div)
  , (0x94, ibinop Rem)
  , (0x95, ibinop And)
  , (0x96, ibinop Or)
  , (0x97, ibinop Xor)
  , (0x98, ibinop Shl)
  , (0x99, ibinop Shr)
  , (0x9a, ibinop UShr)
  , (0x9b, lbinop Add)
  , (0x9c, lbinop Sub)
  , (0x9d, lbinop Mul)
  , (0x9e, lbinop Div)
  , (0x9f, lbinop Rem)
  , (0xa0, lbinop And)
  , (0xa1, lbinop Or)
  , (0xa2, lbinop Xor)
  , (0xa3, lbinop Shl)
  , (0xa4, lbinop Shr)
  , (0xa5, lbinop UShr)
  , (0xa6, fbinop Add)
  , (0xa7, fbinop Sub)
  , (0xa8, fbinop Mul)
  , (0xa9, fbinop Div)
  , (0xaa, fbinop Rem)
  , (0xab, dbinop Add)
  , (0xac, dbinop Sub)
  , (0xad, dbinop Mul)
  , (0xae, dbinop Div)
  , (0xaf, dbinop Rem)
  -- Binary assignment operators
  , (0xb0, ibinopa Add)
  , (0xb1, ibinopa Sub)
  , (0xb2, ibinopa Mul)
  , (0xb3, ibinopa Div)
  , (0xb4, ibinopa Rem)
  , (0xb5, ibinopa And)
  , (0xb6, ibinopa Or)
  , (0xb7, ibinopa Xor)
  , (0xb8, ibinopa Shl)
  , (0xb9, ibinopa Shr)
  , (0xba, ibinopa UShr)
  , (0xbb, lbinopa Add)
  , (0xbc, lbinopa Sub)
  , (0xbd, lbinopa Mul)
  , (0xbe, lbinopa Div)
  , (0xbf, lbinopa Rem)
  , (0xc0, lbinopa And)
  , (0xc1, lbinopa Or)
  , (0xc2, lbinopa Xor)
  , (0xc3, lbinopa Shl)
  , (0xc4, lbinopa Shr)
  , (0xc5, lbinopa UShr)
  , (0xc6, fbinopa Add)
  , (0xc7, fbinopa Sub)
  , (0xc8, fbinopa Mul)
  , (0xc9, fbinopa Div)
  , (0xca, fbinopa Rem)
  , (0xcb, dbinopa Add)
  , (0xcc, dbinopa Sub)
  , (0xcd, dbinopa Mul)
  , (0xce, dbinopa Div)
  , (0xcf, dbinopa Rem)
  -- Binary operators with 16-bit literal arguments
  , (0xd0, binopl16 Add)
  , (0xd1, binopl16 RSub)
  , (0xd2, binopl16 Mul)
  , (0xd3, binopl16 Div)
  , (0xd4, binopl16 Rem)
  , (0xd5, binopl16 And)
  , (0xd6, binopl16 Or)
  , (0xd7, binopl16 Xor)
  -- Binary operators with 8-bit literal arguments
  , (0xd8, binopl8 Add)
  , (0xd9, binopl8 RSub)
  , (0xda, binopl8 Mul)
  , (0xdb, binopl8 Div)
  , (0xdc, binopl8 Rem)
  , (0xdd, binopl8 And)
  , (0xde, binopl8 Or)
  , (0xdf, binopl8 Xor)
  , (0xe0, binopl8 Shl)
  , (0xe1, binopl8 Shr)
  , (0xe2, binopl8 UShr)
  -- Unused
  , (0xe3, InvalidOp)
  , (0xe4, InvalidOp)
  , (0xe5, InvalidOp)
  , (0xe6, InvalidOp)
  , (0xe7, InvalidOp)
  , (0xe8, InvalidOp)
  , (0xe9, InvalidOp)
  , (0xea, InvalidOp)
  , (0xeb, InvalidOp)
  , (0xec, InvalidOp)
  , (0xed, InvalidOp)
  , (0xee, InvalidOp)
  , (0xef, InvalidOp)
  , (0xf0, InvalidOp)
  , (0xf1, InvalidOp)
  , (0xf2, InvalidOp)
  , (0xf3, InvalidOp)
  , (0xf4, InvalidOp)
  , (0xf5, InvalidOp)
  , (0xf6, InvalidOp)
  , (0xf7, InvalidOp)
  , (0xf8, InvalidOp)
  , (0xf9, InvalidOp)
  , (0xfa, InvalidOp)
  , (0xfb, InvalidOp)
  , (0xfc, InvalidOp)
  , (0xfd, InvalidOp)
  , (0xfe, InvalidOp)
  , (0xff, InvalidOp)
  ] where const4 = Const4 . fromIntegral . signExt4
          const16 = Const16 . signExt16 . fromIntegral
          const32 = Const32 . fromIntegral
          constHigh16 = ConstHigh16 . (`shiftL` 16) . fromIntegral
          constWide16 = ConstWide16 . fromIntegral . signExt16 . fromIntegral
          constWide32 = ConstWide32 . signExt32 . fromIntegral
          constWide = ConstWide . fromIntegral
          constWideHigh16 = ConstWideHigh16 . (`shiftL` 48) . fromIntegral
          constString = ConstString . fromIntegral
          unop op = IF12x $ Unop op
          binop c w op = IF23x $ c op w
          binopa c w op = IF12x $ c op w
          ibinop = binop IBinop False
          lbinop = binop IBinop True
          fbinop = binop FBinop False
          dbinop = binop FBinop True
          ibinopa = binopa IBinopAssign False
          lbinopa = binopa IBinopAssign True
          fbinopa = binopa FBinopAssign False
          dbinopa = binopa FBinopAssign True
          binopl16 op = IF22s $ \r1 r2 ->
                        BinopLit16 op r1 r2 . fromIntegral
          binopl8 op = IF22b $ \r1 r2 ->
                       BinopLit8 op r1 r2 . fromIntegral
          cmp op = IF23x $ Cmp op
          ifop op = IF22t $ \r1 r2 -> If op r1 r2 . fromIntegral
          ifzop op = IF21t $ \r -> IfZero op r . fromIntegral
          arrop op = IF23x $ ArrayOp op
          instop op = IF22c $ InstanceFieldOp op
          statop op = IF21c $ StaticFieldOp op
          invoke ty = IF35c $ \f -> Invoke ty False f . map fromIntegral
          invokeRange ty = IF3rc $ Invoke ty True
          ret ty = IF11x $ Return ty . R8
          move1 ty = IF11x $ Move1 ty . R8

fst4, snd4 :: Word8 -> Word4
fst4 = fst . splitWord8
snd4 = snd . splitWord8

fst8, snd8 :: Word16 -> Word8
fst8 = fst . splitWord16
snd8 = snd . splitWord16

combine16 :: (Integral a, Num b, Bits b) => a -> a -> b
combine16 w1 w2 = (fromIntegral w1 `shiftL` 16) .|. fromIntegral w2

combine16' :: (Integral a, Num b, Bits b) => a -> a -> a -> a -> b
combine16' w1 w2 w3 w4 =
  (fromIntegral w1 `shiftL` 48) .|.
  (fromIntegral w2 `shiftL` 32) .|.
  (fromIntegral w3 `shiftL` 16) .|.
  fromIntegral w4

signExt4 :: Word8 -> Int8
signExt4 w = w' .|. (if w' .&. 0x8 /= 0 then 0xF0 else 0)
  where w' = fromIntegral w

signExt16 :: Word32 -> Int32
signExt16 w = w' .|. (if w' .&. 0x8000 /= 0 then 0xFFFF0000 else 0)
  where w' = fromIntegral w

signExt32 :: Int64 -> Int64
signExt32 w =
  w' .|. (if w' .&. 0x80000000 /= 0 then 0xFFFFFFFF00000000 else 0)
    where w' = fromIntegral w

iparser :: Word8 -> IFormatParser
iparser = (!) iparseTable

decodeInstructions :: [Word16] -> Either DecodeError [Instruction]
decodeInstructions [] = return []
decodeInstructions (0x0100 : sz : k : k' : ws) =
  liftM (PackedSwitchData key ts :) $ decodeInstructions ws'
    where key = combine16 k' k
          (ts, ws') = splitAt (2 * fromIntegral sz) ws
decodeInstructions (0x0200 : sz : ws) =
  liftM (SparseSwitchData ks ts :) $ decodeInstructions ws''
    where (ks, ws') = splitAt sz' ws
          (ts, ws'') = splitAt sz' ws'
          sz' = fromIntegral $ 2 * sz
decodeInstructions (0x0300 : esz : sz : sz' : ws) =
  liftM (ArrayData esz size vs :) $ decodeInstructions ws'
    where size = combine16 sz' sz
          count = ((size * fromIntegral esz) + 1) `div` 2
          (vs, ws') = splitAt (fromIntegral count) ws
decodeInstructions (w : ws) = liftM2 (:) insn (decodeInstructions ws'')
  where
    (aa, op) = splitWord16 w
    (b, a) = splitWord8 aa
    (insn, ws'') =
      case (iparser op, ws) of
        (IF10x  f, _) -> (return f, ws)
        (IF12x  f, _) -> (return $ f a b, ws)
        (IF11n  f, _) -> (return $ f a b, ws)
        (IF11x  f, _) -> (return $ f aa, ws)
        (IF10t  f, _) -> (return $ f aa, ws)
        (IF20t  f, w1 : ws') -> (return $ f w1, ws')
        (IF22x  f, w1 : ws') -> (return $ f aa w1, ws')
        (IF21t  f, w1 : ws') -> (return $ f aa w1, ws')
        (IF21s  f, w1 : ws') -> (return $ f aa w1, ws')
        (IF21h  f, w1 : ws') -> (return $ f aa w1, ws')
        (IF21c  f, w1 : ws') -> (return $ f aa w1, ws')
        (IF23x  f, w1 : ws') -> (return $ f aa (snd8 w1) (fst8 w1), ws')
        (IF22b  f, w1 : ws') -> (return $ f aa (snd8 w1) (fst8 w1), ws')
        (IF22t  f, w1 : ws') -> (return $ f a b w1, ws')
        (IF22s  f, w1 : ws') -> (return $ f a b w1, ws')
        (IF22c  f, w1 : ws') -> (return $ f a b w1, ws')
        (IF22cs f, w1 : ws') -> (return $ f a b w1, ws')
        (IF30t  f, w1 : w2 : ws') -> (return $ f (combine16 w2 w1), ws')
        (IF32x  f, w1 : w2 : ws') -> (return $ f w1 w2, ws')
        (IF31i  f, w1 : w2 : ws') -> (return $ f aa (combine16 w2 w1), ws')
        (IF31t  f, w1 : w2 : ws') -> (return $ f aa (combine16 w2 w1), ws')
        (IF31c  f, w1 : w2 : ws') -> (return $ f aa (combine16 w2 w1), ws')
        (IF35c  fn, w1 : w2 : ws') ->
          if b <= 5
            then (return $ fn w1 (take (fromIntegral b) [d, e, f, g, a]), ws')
            else (fail "invalid B value for IF35c encoding", ws')
          where (g, f, e, d) = splitWord16' w2
        (IF3rc  fn, w1 : w2 : ws') ->
          (return $ fn w1 [w2..((w2 + fromIntegral aa) - 1)],  ws')
        (IF51l  fn, w1 : w2 : w3 : w4 : ws') ->
          (return $ fn aa (combine16' w4 w3 w2 w1), ws')
        (InvalidOp, _) -> (fail $ invalidOp op, ws)
        _ -> (fail $ prematureEnd op w, ws)

{-
encodeInstructions :: [Instruction] -> [Word16]
encodeInstructions = undefined
-}
