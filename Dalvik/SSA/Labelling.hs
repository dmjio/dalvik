{-# LANGUAGE ViewPatterns #-}
-- | This module implements SSA value numbering over the low-level Dalvik IR
--
-- The algorithm used is from Braun et al
-- (http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf).  The
-- labelling maps each operand of a low-level Dalvik instruction to
-- the SSA number of that operand.
--
-- Note that SSA numbers are only required for *instructions*.
-- Constants are, well, constant, and can be handled directly during
-- translation.
--
-- Translation to the SSA IR requires knot tying.  Many instructions
-- can be ignored.  Instructions with a destination register get assigned
-- the number placed on their destination (in fact, they create a new value
-- by virtue of having that destination).
--
-- > newinstance r2 Double
--
-- is basically equivalent to
--
-- > r2 <- newinstance Double
--
-- So r2 is a new SSA value here.
--
-- > loop:
-- >   binop Add r1 r1 r2
-- >   br (r1 < 100) loop
--
-- Should translate into something like
-- > loop:
-- >   r4 <- phi(r3, r1)
-- >   r3 <- binop add r4 r2
-- >   br (r4 < 100) loop
module Dalvik.SSA.Labelling (
  -- * Data Types
  Label(..),
  Labelling(..),
  labelInstructions,
  -- * Testing
  prettyLabelling,
  generatedLabelsAreUnique
  ) where

import Control.Monad ( forM_ )
import Control.Monad.Trans.RWS.Strict
import Data.Int ( Int64 )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, isJust, mapMaybe )
import qualified Data.List as L
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word ( Word8, Word16 )
import Text.PrettyPrint as PP
import Text.Printf

import Dalvik.Instruction

data Label = SimpleLabel Int64
           | PhiLabel BlockNumber Int64
           | ArgumentLabel String Int64
           deriving (Eq, Ord, Show)

freshLabel :: SSALabeller Label
freshLabel = do
  s <- get
  put s { labelCounter = labelCounter s + 1 }
  return $ SimpleLabel (labelCounter s)

freshPhi :: BlockNumber -> SSALabeller Label
freshPhi bn = do
  s <- get
  let lid = labelCounter s
      l = PhiLabel bn lid
  put s { labelCounter = lid + 1
        , phiOperands = M.insert l S.empty (phiOperands s)
        }
  return l

-- | A labelling assigns an SSA number/Label to a register at *each*
-- 'Instruction'.
data Labelling =
  Labelling { labellingReadRegs :: Map Instruction (Map Word16 Label)
            , labellingWriteRegs :: Map Instruction Label
            , labellingPhis :: Map Label (Set Label)
            , labellingBasicBlocks :: BasicBlocks
            , labellingInstructions :: Vector Instruction
            }
  deriving (Eq, Ord, Show)

-- | Returns @True@ if all of the labels assigned to instructions
-- are unique.
generatedLabelsAreUnique :: Labelling -> Bool
generatedLabelsAreUnique =
  snd . foldr checkRepeats (S.empty, True) . M.toList . labellingWriteRegs
  where
    checkRepeats (_, l) (marked, foundRepeat)
      | S.member l marked = (marked, False)
      | otherwise = (S.insert l marked, foundRepeat)

-- Blocks need to end after branches/switches/invokes/throws

data LabelState =
  LabelState { currentDefinition :: Map Word16 (Map BlockNumber Label)
               -- ^ We track the current definition of each register at
               -- each block.
             , instructionLabels :: Map Instruction (Map Word16 Label)
               -- ^ This is separate from
               -- @currentDefinition@ because that
               -- field is mutated.
             , instructionResultLabels :: Map Instruction Label
               -- ^ Store the label of instructions that produce
               -- a result separately from the normal map.  This is only
               -- really required because of the compound instructions
               -- 'IBinopAssign' and 'FBinopAssign'
             , phiOperands :: Map Label (Set Label)
             , labelCounter :: Int64
             }

data LabelEnv =
  LabelEnv { envInstructionStream :: Vector Instruction
           , envBasicBlocks :: BasicBlocks
             -- ^ Information about basic blocks in the instruction
             -- stream
           }

data BasicBlocks =
  BasicBlocks { bbBlocks :: Vector (BlockNumber, Vector Instruction)
              , bbFromInstruction :: Vector BlockNumber
                -- ^ One entry per instruction, the reverse mapping
              , bbPredecessors :: Vector [BlockNumber]
                -- ^ One entry per basic block
              , bbBlockEnds :: IntSet
                -- ^ Ends of all block ranges.  We need these because
                -- some blocks end on implicit fallthrough on
                -- instructions that are *not* terminators.
              }
  deriving (Eq, Ord, Show)

-- | Get the basic block ID for the instruction at the given index.
blockForInstruction :: Int -> SSALabeller BlockNumber
blockForInstruction ix = do
  ivec <- asks (bbFromInstruction . envBasicBlocks)
  let Just bnum = ivec V.!? ix
  return bnum

-- Builds up an implicit CFG, only recording block predecessors.
emptyEnv :: Vector Instruction -> LabelEnv
emptyEnv ivec =
  LabelEnv { envInstructionStream = ivec
           , envBasicBlocks = bbs
           }
  where
    preds = buildPredecessors ivec bvec bmapVec blockEnds
    (bvec, bnumMap) = splitIntoBlocks ivec
    bmapVec = buildInstructionBlockMap bnumMap
    bbs = BasicBlocks { bbBlocks = bvec
                      , bbFromInstruction = bmapVec
                      , bbPredecessors = preds
                      , bbBlockEnds = blockEnds
                      }
    blockEnds = M.foldrWithKey collectEnds IS.empty bnumMap
    collectEnds (_, e) _ = IS.insert e

-- | Create a new empty state.  This state is initialized to account
-- for method arguments, which occupy the last @length argRegs@
-- registers.  Wide arguments take two registers, but we only need to
-- worry about the first register in the pair (since the second is
-- never explicitly accessed).
emptyLabelState :: [(String, Word16)] -> LabelState
emptyLabelState argRegs =
  LabelState { currentDefinition = fst $ L.foldl' addArgumentMapping (M.empty, 0) argRegs
             , instructionLabels = M.empty
             , instructionResultLabels = M.empty
             , phiOperands = M.empty
             , labelCounter = fromIntegral $ length argRegs
             }
  where
    addArgumentMapping (m, valNo) (argName, regno) =
      let l = ArgumentLabel argName valNo
      in (M.insertWith M.union regno (M.singleton 0 l) m, valNo + 1)

-- | Build a vector of Instructions for fast indexing from branch
-- instructions.  The argument/register mapping should be resolved
-- outside of this function - it only cares about which registers
-- are occupied by arguments.
--
-- While wide arguments take two registers, the input mapping need
-- only record the *first* register occupied.
labelInstructions :: [(String, Word16)]
                     -- ^ A mapping of argument names to the register numbers
                     -> [Instruction]
                     -- ^ The instruction stream for the method
                     -> Labelling
labelInstructions argRegs is = fst $ evalRWS label' e0 s0
  where
    s0 = emptyLabelState argRegs
    e0 = emptyEnv ivec
    ivec = V.fromList is


label' :: SSALabeller Labelling
label' = do
  ivec <- asks envInstructionStream
  mapM_ labelInstruction $ V.toList $ V.indexed ivec
  s <- get
  bbs <- asks envBasicBlocks
  return $ Labelling { labellingReadRegs = instructionLabels s
                     , labellingWriteRegs = instructionResultLabels s
                     , labellingPhis = phiOperands s
                     , labellingBasicBlocks = bbs
                     , labellingInstructions = ivec
                     }


type BlockNumber = Int

type SSALabeller = RWS LabelEnv () LabelState


-- | This is the main part of the algorithm from the paper.  Each
-- instruction is processed separately.  Apply *read* before *write*
-- rules.  This will update the per-instruction register map.  That
-- map will be used for the translation step.
labelInstruction :: (Int, Instruction) -> SSALabeller ()
labelInstruction (ix, inst) = do
  instBlock <- blockForInstruction ix
  let rr :: (FromRegister a) => a -> SSALabeller ()
      rr = recordReadRegister inst instBlock
      rw :: (FromRegister a) => a -> SSALabeller ()
      rw = recordWriteRegister inst instBlock
      rrs :: (FromRegister a) => [a] -> SSALabeller ()
      rrs = mapM_ rr
  case inst of
    Nop -> return ()
    -- Move isn't quite like other instructions.  It is a
    -- register-register move, and doesn't create a new value.  It
    -- just copies the contents of one register to another as a side
    -- effect.  The @rw@ function only applies to instructions that
    -- generate a new SSA value.
    --
    -- In terms of implementation, we do *not* want to make
    -- an entry in the instructionResultLabels table.
    Move _ dst src -> do
      l <- readRegister src instBlock
      recordAssignment inst src l
      writeRegisterLabel dst instBlock l
      recordAssignment inst dst l

    -- This is a pseudo-move that takes an item off of the stack
    -- (following a call or other special instruction) and stuffs it
    -- into a register.  This brings a new value into existence,
    -- unlike normal move.
    Move1 _ dst -> rw dst
    ReturnVoid -> return ()
    Return _ src -> rr src
    LoadConst dst _ -> rw dst
    MonitorEnter src -> rr src
    MonitorExit src -> rr src
    CheckCast src _ -> rr src
    InstanceOf dst src _ -> rr src >> rw dst
    ArrayLength dst src -> rr src >> rw dst
    NewInstance dst _ -> rw dst
    NewArray dst src _ -> rr src >> rw dst
    FilledNewArray _ srcs -> rrs srcs
    FilledNewArrayRange _ srcs -> rrs srcs
    FillArrayData src _ -> rr src
    Throw src -> rr src
    Goto _ -> return ()
    Goto16 _ -> return ()
    Goto32 _ -> return ()
    PackedSwitch src _ -> rr src
    SparseSwitch src _ -> rr src
    Cmp _ dst src1 src2 -> rrs [src1, src2] >> rw dst
    If _ src1 src2 _ -> rrs [src1, src2]
    IfZero _ src _ -> rr src
    ArrayOp (Get _) dst src1 src2 -> rrs [src1, src2] >> rw dst
    ArrayOp (Put _) src3 src1 src2 -> rrs [src1, src2, src3]
    InstanceFieldOp (Get _) dst src _ -> rr src >> rw dst
    InstanceFieldOp (Put _) src2 src1 _ -> rrs [src1, src2]
    StaticFieldOp (Get _) dst _ -> rw dst
    StaticFieldOp (Put _) src _ -> rr src
    Invoke _ _ _ srcs -> rrs srcs
    Unop _ dst src -> rr src >> rw dst
    IBinop _ _ dst src1 src2 -> rrs [src1, src2] >> rw dst
    FBinop _ _ dst src1 src2 -> rrs [src1, src2] >> rw dst
    -- These two read and write from the dest register
    IBinopAssign _ _ dst src -> rrs [src, dst] >> rw dst
    FBinopAssign _ _ dst src -> rrs [src, dst] >> rw dst
    BinopLit16 _ dst src _ -> rr src >> rw dst
    BinopLit8 _ dst src _ -> rr src >> rw dst
    PackedSwitchData _ _ -> return ()
    SparseSwitchData _ _ -> return ()
    ArrayData _ _ _ -> return ()

recordReadRegister :: (FromRegister a) => Instruction -> BlockNumber -> a -> SSALabeller ()
recordReadRegister inst instBlock srcReg = do
  lbl <- readRegister srcReg instBlock
  recordAssignment inst srcReg lbl

recordWriteRegister :: (FromRegister a) => Instruction -> BlockNumber -> a -> SSALabeller ()
recordWriteRegister inst instBlock dstReg = do
  lbl <- writeRegister dstReg instBlock
  modify (addAssignment lbl)
  where
    addAssignment lbl s =
      let lbls = instructionResultLabels s
      in s { instructionResultLabels = M.insert inst lbl lbls }

recordAssignment :: (FromRegister a)
                    => Instruction
                    -> a
                    -> Label
                    -> SSALabeller ()
recordAssignment inst (fromRegister -> reg) lbl =
  modify addAssignment
  where
    addAssignment s =
      let lbls = instructionLabels s
      in s { instructionLabels = M.insertWith M.union inst (M.singleton reg lbl) lbls }

-- | Used for instructions that write to a register.  These always
-- define a new value.  From the paper, this is:
--
-- >  writeVariable(variable, block, value):
-- >    currentDef[variable][block] ← value
writeRegister :: (FromRegister a) => a -> BlockNumber -> SSALabeller Label
writeRegister (fromRegister -> reg) block = do
  l <- freshLabel
  writeRegisterLabel reg block l
  return l

writeRegisterLabel :: (FromRegister a) => a -> BlockNumber -> Label -> SSALabeller ()
writeRegisterLabel (fromRegister -> reg) block l = do
  s <- get
  let defs' = M.insertWith M.union reg (M.singleton block l) (currentDefinition s)
  put s { currentDefinition = defs' }


-- | Find the label for a register being read from.  If we have a
-- local definition (due to local variable numbering, i.e., a write in
-- the current block), return that.  Otherwise, check for a global
-- variable numbering.
readRegister :: (FromRegister a) => a -> BlockNumber -> SSALabeller Label
readRegister (fromRegister -> reg) block = do
  s <- get
  case M.lookup reg (currentDefinition s) of
    Nothing -> readRegisterRecursive reg block
    Just varDefs ->
      case M.lookup block varDefs of
        Nothing -> readRegisterRecursive reg block
        Just label -> return label

-- | This is global variable numbering (global in the sense of not the
-- local block of the instruction).  If a block has a single
-- predecessor, the label is retrieved from the predecessor block.
-- Otherwise, we need some phi magic.
--
-- There are calls to write here to let us memoize lookups.
--
-- Note: If there are no predecessors, it is actually not defined.  We
-- would probably want to know about that... maybe just error for now.
--
-- Note 2: This implementation doesn't use the 'sealed' code from the
-- paper because we have a complete CFG.
readRegisterRecursive :: Word16 -> BlockNumber -> SSALabeller Label
readRegisterRecursive reg block = do
  preds <- basicBlockPredecessors block
  l <- case preds of
    [singlePred] -> readRegister reg singlePred
    _ -> do
      p <- freshPhi block
      writeRegisterLabel reg block p
      addPhiOperands reg p preds
  writeRegisterLabel reg block l
  return l

addPhiOperands :: Word16 -> Label -> [BlockNumber] -> SSALabeller Label
addPhiOperands reg phi preds = do
  forM_ preds $ \p -> do
    l <- readRegister reg p
    appendPhiOperand phi l
  -- FIXME: tryRemoveTrivialPhi here.  Get everything else working
  -- first, though.
  return phi

appendPhiOperand :: Label -> Label -> SSALabeller ()
appendPhiOperand phi operand = modify appendOperand
  where
    appendOperand s = s { phiOperands = M.insertWith S.union phi (S.singleton operand) (phiOperands s) }

-- Block predecessors

basicBlockPredecessors :: BlockNumber -> SSALabeller [BlockNumber]
basicBlockPredecessors b = do
  pmap <- asks (bbPredecessors . envBasicBlocks)
  let Just ps = pmap V.!? b
  return ps

-- Iterate over the block list and find the targets of the terminator.
-- Build up a reverse Map and then transform it to a Vector at the end.
--
-- FIXME: The problem here is that it doesn't account for fallthrough
-- into an implicit block (e.g., the *target* of a jump instruction).
-- Save these from an earlier step so that they can be used here to
-- build an accurate predecessor list.
buildPredecessors :: Vector Instruction
                     -> Vector (BlockNumber, Vector Instruction)
                     -> Vector BlockNumber
                     -> IntSet
                     -> Vector [BlockNumber]
buildPredecessors ivec bvec bmap blockEnds =
  V.generate (V.length bvec) getPreds
  where
    getPreds ix = S.toList $ fromMaybe S.empty $ M.lookup ix predMap
    predMap = V.ifoldl' addTermSuccs M.empty ivec
    addTermSuccs m ix inst
      -- @inst@ is a fallthrough instruction that implicitly ends a block,
      -- so we need to make an entry for it.  We don't make an entry for the
      -- last instruction in the function.
      | not (isTerminator ivec ix inst) &&
        IS.member ix blockEnds &&
        ix < V.length ivec - 1 =
        let target = ix + 1
            Just termBlock = bmap V.!? ix
            Just targetBlock = bmap V.!? target
        in M.insertWith S.union targetBlock (S.singleton termBlock) m
      | otherwise =
        case terminatorAbsoluteTargets ivec ix inst of
          Nothing -> m
          Just targets ->
            let Just termBlock = bmap V.!? ix
                targetBlocks = mapMaybe (bmap V.!?) targets
                addPreds a targetBlock = M.insertWith S.union targetBlock (S.singleton termBlock) a
            in L.foldl' addPreds m targetBlocks

buildInstructionBlockMap :: Map (Int, Int) BlockNumber -> Vector BlockNumber
buildInstructionBlockMap =
  V.fromList . map snd . L.sort . concatMap fromRange . M.toList
  where
    fromRange ((start, end), bnum) = zip [start..end] (repeat bnum)


-- Splitting into blocks

-- Scan through the instruction vector and end a block if either: 1) @ix@ is a terminator
-- or 2) @ix + 1@ starts a block.
splitIntoBlocks :: Vector Instruction
                   -> (Vector (BlockNumber, Vector Instruction), Map (Int, Int) BlockNumber)
splitIntoBlocks ivec = (V.indexed (V.fromList (reverse blocks)), blockRanges)
  where
    (blocks, blockRanges, _) = V.ifoldl' splitInstrs ([], M.empty, blockBeginnings) ivec
    blockBeginnings = V.ifoldl' (addTargetIndex ivec) (IS.singleton 0) ivec
    splitInstrs :: ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
                   -> Int
                   -> Instruction
                   -> ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
    splitInstrs acc@(bs, ranges, blockStarts) ix inst
      | isTerminator ivec ix inst || (ix + 1) `IS.member` blockStarts =
        let len = ix - blockStart + 1
        in (V.slice blockStart len ivec : bs,
            M.insert (blockStart, ix) bnum ranges,
            blockStarts')
      | otherwise = acc
      where
        (blockStart, blockStarts') = IS.deleteFindMin blockStarts
        bnum = M.size ranges

-- | If the instruction is a jump, add all of its possible target
-- indices (absolute, based from 0) to the Set.  While data loss is
-- technically possible because of a conversion from 'Int32' to 'Int',
-- that isn't a problem in practice because we wouldn't have enough
-- RAM for an array big enough to suffer from the loss.
--
-- Note that conditional branch instructions actually have two
-- targets: the explicit one and the implicit fallthrough target.
-- The fallthrough needs to begin a new block, too.
addTargetIndex :: Vector Instruction -> IntSet -> Int -> Instruction -> IntSet
addTargetIndex ivec acc ix inst =
  case terminatorAbsoluteTargets ivec ix inst of
    Nothing -> acc
    Just targets -> L.foldl' (flip IS.insert) acc targets

-- | Find the absolute target indices (into the instruction vector) for each
-- block terminator instruction.  The conditional branches have explicit
-- targets, but can also allow execution to fall through.
--
-- FIXME: Invoke *can* be a terminator if it is in a try block.  We do
-- need to know that here, technically.  Actually, any invoke or instruction
-- touching a reference can get an edge to an exception handler...
--
-- FIXME: Additionally, if the next instruction is the target of a branch,
-- then this instruction is a block terminator with a single (fallthrough) target.
terminatorAbsoluteTargets :: Vector Instruction -> Int -> Instruction -> Maybe [Int]
terminatorAbsoluteTargets ivec ix inst =
  case inst of
    Goto i8 -> Just [fromIntegral i8 + ix]
    Goto16 i16 -> Just [fromIntegral i16 + ix]
    Goto32 i32 -> Just [fromIntegral i32 + ix]
    PackedSwitch _ tableOff ->
      let Just (PackedSwitchData _ offs) = ivec V.!? (fromIntegral tableOff + ix)
      in Just (ix + 1 : [fromIntegral o + ix | o <- offs])
    SparseSwitch _ tableOff ->
      let Just (SparseSwitchData _ offs) = ivec V.!? (fromIntegral tableOff + ix)
      in Just (ix + 1 : [fromIntegral o + ix | o <- offs])
    If _ _ _ off -> Just [ix + 1, fromIntegral off + ix]
    IfZero _ _ off -> Just [ix + 1, fromIntegral off + ix]
    ReturnVoid -> Just []
    Return _ _ -> Just []
    Throw _ -> Just []
    _ -> Nothing -- FIXME: Take additional information about exception tables

isTerminator :: Vector Instruction -> Int -> Instruction -> Bool
isTerminator ivec ix inst =
  isJust $ terminatorAbsoluteTargets ivec ix inst

-- | This is a simple helper class to convert from Register
-- identifiers in the low-level IR to a consistent Word16.  The spec
-- defines register numbers as < 2^16.  This is a safer convenience so
-- that we don't have to just use a more general (Num a) constraint.
class FromRegister a where
  fromRegister :: a -> Word16

instance FromRegister Word16 where
  fromRegister = id

instance FromRegister Word8 where
  fromRegister = fromIntegral

instance FromRegister Reg where
  fromRegister (R4 r) = fromRegister r
  fromRegister (R8 r) = fromRegister r
  fromRegister (R16 r) = fromRegister r


-- Pretty printing for debugging

phiForBlock :: BlockNumber -> Label -> Bool
phiForBlock bid l =
  case l of
    PhiLabel phiBlock _ -> phiBlock == bid
    _ -> False

-- | Get the block number for an instruction
instructionBlockNumber :: BasicBlocks -> Int -> Maybe BlockNumber
instructionBlockNumber bbs = (bbFromInstruction bbs V.!?)

prettyLabelling :: Labelling -> String
prettyLabelling l =
  render $ PP.vcat $ map prettyBlock $ V.toList $ bbBlocks bbs
  where
    bbs = labellingBasicBlocks l
    ivec = labellingInstructions l
    prettyBlock (bid, insts) =
      let header = PP.text ";; " <> PP.int bid
          blockPhis = filter (phiForBlock bid . fst) $ M.toList (labellingPhis l)
          blockPhiDoc = PP.vcat [ PP.text (printf "$%d = phi(%s)" phiL (show vals))
                                | (PhiLabel _ phiL, (S.toList -> vals)) <- blockPhis
                                ]
          body = blockPhiDoc $+$ (PP.vcat $ map prettyInst $ V.toList insts)
      in header $+$ PP.nest 2 body
    branchTargets i = fromMaybe "??" $ do
      ix <- V.elemIndex i ivec
      ts <- terminatorAbsoluteTargets ivec ix i
      targetBlocks <- mapM (instructionBlockNumber bbs) ts
      return $ L.intercalate ", " (map show targetBlocks)
    prettyInst i =
      case i of
        Nop -> PP.text ";; nop"
        Move t r1 r2 -> PP.text $ printf ";; move %s %s %s" (show t) (show r1) (show r2)
        Move1 t r -> PP.text $ printf "%s = move1 %s ;\t (move1 %s %s)" (wLabelId r) (show t) (show t) (show r)
        ReturnVoid -> PP.text "ret"
        Return _ r -> PP.text $ printf "ret $%s ;\t (ret %s)" (rLabelId r) (show r)
        LoadConst r arg -> PP.text $ printf "$%s = loadc %s ;\t (loadc %s %s)" (wLabelId r) (show arg) (show r) (show arg)
        MonitorEnter r -> PP.text $ printf "menter $%s ;\t (menter %s)" (rLabelId r) (show r)
        MonitorExit r -> PP.text $ printf "mexit $%s ;\t (mexit %s)" (rLabelId r) (show r)
        CheckCast r t -> PP.text $ printf "checkcast $%s %s ;\t (checkcast %s %s)" (rLabelId r) (show t) (show r) (show t)
        InstanceOf d s t -> PP.text $ printf "$%s = instanceof $%s %s ;\t (instanceof %s %s %s)" (wLabelId d) (rLabelId s) (show t) (show d) (show s) (show t)
        ArrayLength d s -> PP.text $ printf "$%s = arraylength $%s ;\t (arraylength %s %s)" (wLabelId d) (rLabelId s) (show d) (show s)
        NewInstance d t -> PP.text $ printf "$%s = newinstance $%s ;\t (newinstance %s %s)" (wLabelId d) (show t) (show d) (show t)
        NewArray d s t -> PP.text $ printf "$%s = newarray $%s $%s ;\t (newarray %s %s %s)" (wLabelId d) (rLabelId s) (show t) (show d) (show s) (show t)
        FilledNewArray t srcs -> PP.text $ printf "fillednewarray %s %s ;\t (fillednewarray %s %s)" (show t) (concatMap rLabelId srcs) (show t) (show srcs)
        FilledNewArrayRange t srcs -> PP.text $ printf "fillednewarrayrange %s %s ;\t (fillednewarrayrange %s %s)" (show t) (concatMap rLabelId srcs) (show t) (show srcs)
        FillArrayData r off -> PP.text $ printf "fillarraydata $%s %s ;\t (fillarraydata %s %s)" (rLabelId r) (show off) (show r) (show off)
        Throw r -> PP.text $ printf "throw $%s ;\t (throw %s)" (rLabelId r) (show r)
        Cmp op d s1 s2 -> PP.text $ printf "$%s <- cmp %s $%s $%s ;\t (cmp %s %s %s %s)" (wLabelId d) (show op) (rLabelId s1) (rLabelId s2) (show op) (show d) (show s1) (show s2)
        ArrayOp (Get _) dst src1 src2 -> PP.text $ printf "$%s = getarray $%s $%s ;\t (getarray %s %s %s)" (wLabelId dst) (rLabelId src1) (rLabelId src2) (show dst) (show src1) (show src2)
        ArrayOp (Put _) src3 src1 src2 -> PP.text $ printf "putarray $%s $%s $%s ;\t (putarray %s %s %s)" (rLabelId src3) (rLabelId src1) (rLabelId src2) (show src3) (show src1) (show src2)
        InstanceFieldOp (Get _) dst src f -> PP.text $ printf "$%s = getfield $%s %s ;\t (getfield %s %s %s)" (wLabelId dst) (rLabelId src) (show f) (show dst) (show src) (show f)
        InstanceFieldOp (Put _) src2 src1 f -> PP.text $ printf "putfield $%s $%s %s ;\t (putfield %s %s %s)" (rLabelId src2) (rLabelId src1) (show f) (show src2) (show src1) (show f)
        StaticFieldOp (Get _) dst f -> PP.text $ printf "$%s = getstatic %s ;\t (getstatic %s %s)" (wLabelId dst) (show f) (show dst) (show f)
        StaticFieldOp (Put _) src f -> PP.text $ printf "putstatic $%s %s ;\t (putstatic %s %s)" (rLabelId src) (show f) (show src) (show f)
        Invoke kind _ method srcs -> PP.text $ printf "invoke %s [%s] %s ;\t (%s)" (show kind) (show method) (show (map rLabelId srcs)) (show srcs)
        Unop op d s -> PP.text $ printf "$%s <- %s $%s ;\t (%s %s %s)" (wLabelId d) (show op) (rLabelId s) (show op) (show d) (show s)
        IBinop op _ d s1 s2 -> PP.text $ printf "$%s = %s $%s $%s ;\t (%s[i] %s %s %s)" (wLabelId d) (show op) (rLabelId s1) (rLabelId s2) (show op) (show d) (show s1) (show s2)
        FBinop op _ d s1 s2 -> PP.text $ printf "$%s = %s $%s $%s ;\t (%s[f] %s %s %s)" (wLabelId d) (show op) (rLabelId s1) (rLabelId s2) (show op) (show d) (show s1) (show s2)
        IBinopAssign op _ sd s -> PP.text $ printf "$%s = %s $%s $%s ;\t (%s[ia] %s %s)" (wLabelId sd) (show op) (rLabelId sd) (rLabelId s) (show op) (show sd) (show s)
        FBinopAssign op _ sd s -> PP.text $ printf "$%s = %s $%s $%s ;\t (%s[fa] %s %s)" (wLabelId sd) (show op) (rLabelId sd) (rLabelId s) (show op) (show sd) (show s)
        BinopLit16 op d s lit -> PP.text $ printf "$%s = %s $%s %d ;\t (%s %s %s %d)" (wLabelId d) (show op) (rLabelId s) lit (show op) (show d) (show s) lit
        BinopLit8 op d s lit -> PP.text $ printf "$%s = %s $%s %d ;\t (%s %s %s %d)" (wLabelId d) (show op) (rLabelId s) lit (show op) (show d) (show s) lit
        PackedSwitchData _ _ -> PP.text ";; packedswitchdata"
        SparseSwitchData _ _ -> PP.text ";; sparseswitchdata"
        ArrayData _ _ _ -> PP.text ";; arraydata"
        Goto _ -> PP.text $ printf "goto %s" (branchTargets i)
        Goto16 _ -> PP.text $ printf "goto %s" (branchTargets i)
        Goto32 _ -> PP.text $ printf "goto %s" (branchTargets i)
        PackedSwitch src _ -> PP.text $ printf "switch $%s %s" (rLabelId src) (branchTargets i)
        SparseSwitch src _ -> PP.text $ printf "switch $%s %s" (rLabelId src) (branchTargets i)
        IfZero op src _ -> PP.text $ printf "if0 %s $%s %s" (show op) (rLabelId src) (branchTargets i)
        If op src1 src2 _ -> PP.text $ printf "if %s $%s $%s %s" (show op) (rLabelId src1) (rLabelId src2) (branchTargets i)
      where
        rLabelId reg = fromMaybe "??" $ do
          regMap <- M.lookup i (labellingReadRegs l)
          lab <- M.lookup (fromRegister reg) regMap
          case lab of
            SimpleLabel lnum -> return (show lnum)
            PhiLabel _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return s
        wLabelId _reg = fromMaybe "??" $ do
          lab <- M.lookup i (labellingWriteRegs l)
          case lab of
            SimpleLabel lnum -> return (show lnum)
            PhiLabel _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return s
