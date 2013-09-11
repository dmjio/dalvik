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
module Dalvik.SSA.Labelling where

import Control.Monad.Trans.State.Strict
import Data.Int ( Int64 )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.List as L
import Data.Vector ( Vector )
import qualified Data.Vector as V

import Dalvik.Instruction

data Label = SimpleLabel Int64
           | PhiLabel Int64 [Label]

-- | A labelling assigns an SSA number/Label to a register at *each*
-- 'Instruction'.
data Labelling = Labelling (Map Instruction (Map Reg Label))

-- Blocks need to end after branches/switches/invokes/throws

data LabelState = LabelState { currentDefinition :: Map Reg (Map Int Label)
                             }

emptyLabelState :: LabelState
emptyLabelState = LabelState { currentDefinition = M.empty
                             }

-- | Build a vector of Instructions for fast indexing from branch
-- instructions.
labelInstructions :: [Instruction] -> Labelling
labelInstructions is = evalState (label' ivec bvec) emptyLabelState
  where
    ivec = V.fromList is
    bvec = splitIntoBlocks ivec

-- | Split an instruction stream into basic blocks.  Each block is
-- just a constant-time slice of the input instruction stream.
--
-- The end of a block is a jump instruction (terminator).  The
-- beginnings of blocks can only be determined after examining all
-- jumps since there are no markers in the instruction stream.
splitIntoBlocks :: Vector Instruction -> Vector (Int, Vector Instruction)
splitIntoBlocks ivec = V.indexed $ V.fromList $ reverse blockChunks
  where
    (_, blockChunks) = V.ifoldl' (makeBlockIfTerminator ivec) (blockBeginnings, []) ivec
    -- Zero is an implicit block beginning since execution starts there.
    blockBeginnings = V.ifoldl' addTargetIndex (IS.singleton 0) ivec

makeBlockIfTerminator :: Vector Instruction
                         -> (IntSet, [Vector Instruction])
                         -> Int
                         -> Instruction
                         -> (IntSet, [Vector Instruction])
makeBlockIfTerminator ivec acc@(blockStarts, blocks) ix inst
  | null (terminatorAbsoluteTargets ix inst) = acc
  | otherwise =
    let (blockStart, blockStarts') = IS.deleteFindMin blockStarts
        len = ix - blockStart
    in (blockStarts', V.slice blockStart len ivec : blocks)

-- | If the instruction is a jump, add all of its possible target
-- indices (absolute, based from 0) to the Set.  While data loss is
-- technically possible because of a conversion from 'Int32' to 'Int',
-- that isn't a problem in practice because we wouldn't have enough
-- RAM for an array big enough to suffer from the loss.
addTargetIndex :: IntSet -> Int -> Instruction -> IntSet
addTargetIndex acc ix inst =
  L.foldl' (flip IS.insert) acc (terminatorAbsoluteTargets ix inst)

-- |
-- FIXME: Invoke *can* be a terminator if it is in a try block.  We do
-- need to know that here, technically.
terminatorAbsoluteTargets :: Int -> Instruction -> [Int]
terminatorAbsoluteTargets ix inst =
  case inst of
    Goto i8 -> [fromIntegral i8 + ix]
    Goto16 i16 -> [fromIntegral i16 + ix]
    Goto32 i32 -> [fromIntegral i32 + ix]
    PackedSwitch _ tableOff -> undefined
    SparseSwitch _ tableOff -> undefined
    If _ _ _ off -> [fromIntegral off + ix]
    IfZero _ _ off -> [fromIntegral off + ix]
    _ -> []

{-
type BlockNumber = Int

label' :: Vector Instruction -> [(BlockNumber, [Instruction])] -> State LabelState Labelling
label' = do
  mapM_ labelBlock
-}
label' = undefined
