-- | Split an 'Instruction' stream into basic blocks.
--
-- This is a support module for the SSA labelling module.
--
-- The instruction stream does not make basic blocks manifest.  Blocks
-- end at terminator instructions, but the beginnings of blocks are
-- more complicated.  If an instruction is the target of a jump
-- instruction, it begins a basic block.  Furthermore, implicit
-- fallthroughs (e.g., from a conditional branch) also introduce new
-- block beginnings.
--
-- This module attempts to abstract all of these details in a
-- reasonable way.
module Dalvik.SSA.BasicBlocks (
  BasicBlocks,
  BlockNumber,
  TypeName,
  ExceptionRange(..),
  basicBlockForInstruction,
  basicBlockPredecessors,
  basicBlockSuccessors,
  basicBlocksAsList,
  findBasicBlocks,
  instructionBlockNumber,
  instructionEndsBlock
  ) where

import qualified Data.ByteString as BS
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, isJust, mapMaybe )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word ( Word32, Word16 )

import Dalvik.Instruction

-- | Types of Dalvik Type names
type TypeName = BS.ByteString

-- | Descriptions of exception handlers within a method.  Each of the
-- Word32 fields is an offset into the instruction stream (0-indexed).
data ExceptionRange =
  ExceptionRange { erOffset :: Word32
                 , erCount :: Word16
                 , erCatch :: [(BS.ByteString, Word32)]
                 , erCatchAll :: Maybe Word32
                 }

-- | Unique (within a method) basic block identifiers
type BlockNumber = Int

-- | An opaque abstraction of basic blocks for the low-level Dalvik IR (Dalvik.Instruction)
data BasicBlocks =
  BasicBlocks { bbBlocks :: Vector (BlockNumber, Vector Instruction)
                -- ^ The actual Basic Blocks.  These are *slices* out
                -- of the original Instruction vector.
              , bbFromInstruction :: Vector BlockNumber
                -- ^ One entry per instruction, the reverse mapping
              , bbPredecessors :: Vector [BlockNumber]
                -- ^ One entry per basic block
              , bbSuccessors :: Vector [BlockNumber]
              , bbBlockEnds :: IntSet
                -- ^ Ends of all block ranges.  We need these because
                -- some blocks end on implicit fallthrough on
                -- instructions that are *not* terminators.
              }
  deriving (Eq, Ord, Show)

-- | Extract the basic blocks into a more directly-usable form.
basicBlocksAsList :: BasicBlocks -> [(BlockNumber, Vector Instruction)]
basicBlocksAsList = V.toList . bbBlocks

-- | Examine the instruction stream and break it into basic blocks.
-- This includes control flow information (block predecessors and
-- successors).
findBasicBlocks :: Vector Instruction -> [ExceptionRange] -> BasicBlocks
findBasicBlocks ivec ers =
  BasicBlocks { bbBlocks = bvec
              , bbFromInstruction = bmapVec
              , bbPredecessors = preds
              , bbSuccessors = succs
              , bbBlockEnds = blockEnds
              }
  where
    (preds, succs) = buildPredecessors ivec bvec bmapVec blockEnds
    (bvec, bnumMap) = splitIntoBlocks ivec
    bmapVec = buildInstructionBlockMap bnumMap
    blockEnds = M.foldrWithKey collectEnds IS.empty bnumMap
    collectEnds (_, e) _ = IS.insert e

-- | Test if the instruction at the given index into the instruction
-- stream ends a basic block.  This covers both explicit block ends
-- (due to terminator instructions) *and* implicit block ends.  An
-- implicit block end arises when the next instruction is the target
-- of a jump (and thus must be the first instruction in a new basic
-- block).
instructionEndsBlock :: BasicBlocks -> Int -> Bool
instructionEndsBlock bbs ix = IS.member ix $ bbBlockEnds bbs

-- | Get the block number for an instruction
instructionBlockNumber :: BasicBlocks -> Int -> Maybe BlockNumber
instructionBlockNumber bbs = (bbFromInstruction bbs V.!?)

-- | Get the basic block ID for the instruction at the given index.
basicBlockForInstruction :: BasicBlocks -> Int -> BlockNumber
basicBlockForInstruction bbs ix =
  let Just bnum = bbFromInstruction bbs V.!? ix
  in bnum

basicBlockPredecessors :: BasicBlocks -> BlockNumber -> [BlockNumber]
basicBlockPredecessors bbs bid =
  let Just ps = bbPredecessors bbs V.!? bid
  in ps

basicBlockSuccessors :: BasicBlocks -> BlockNumber -> [BlockNumber]
basicBlockSuccessors bbs bid =
  let Just ss = bbSuccessors bbs V.!? bid
  in ss

-- Iterate over the block list and find the targets of the terminator.
-- Build up a reverse Map and then transform it to a Vector at the end.
buildPredecessors :: Vector Instruction
                     -> Vector (BlockNumber, Vector Instruction)
                     -> Vector BlockNumber
                     -> IntSet
                     -> (Vector [BlockNumber], Vector [BlockNumber])
buildPredecessors ivec bvec bmap blockEnds =
  (V.generate (V.length bvec) getPreds,
   V.generate (V.length bvec) getSuccs)
  where
    getPreds ix = S.toList $ fromMaybe S.empty $ M.lookup ix predMap
    getSuccs ix = S.toList $ fromMaybe S.empty $ M.lookup ix succMap
    (predMap, succMap) = V.ifoldl' addTermSuccs (M.empty, M.empty) ivec
    addTermSuccs m@(pm, sm) ix inst
      -- @inst@ is a fallthrough instruction that implicitly ends a block,
      -- so we need to make an entry for it.  We don't make an entry for the
      -- last instruction in the function.
      | not (isTerminator ivec ix inst) &&
        IS.member ix blockEnds &&
        ix < V.length ivec - 1 =
        let target = ix + 1
            Just termBlock = bmap V.!? ix
            Just targetBlock = bmap V.!? target
        in (M.insertWith S.union targetBlock (S.singleton termBlock) pm,
            M.insertWith S.union termBlock (S.singleton targetBlock) sm)
      | otherwise =
        case terminatorAbsoluteTargets ivec ix inst of
          Nothing -> m
          Just targets ->
            let Just termBlock = bmap V.!? ix
                targetBlocks = mapMaybe (bmap V.!?) targets
                addSuccsPreds (p, s) targetBlock = (M.insertWith S.union targetBlock (S.singleton termBlock) p,
                                                    M.insertWith S.union termBlock (S.singleton targetBlock) s)
            in L.foldl' addSuccsPreds m targetBlocks

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

{- Note [Exceptional Control Flow]



-}
