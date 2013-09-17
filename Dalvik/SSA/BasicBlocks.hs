{-# LANGUAGE OverloadedStrings #-}
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
import Data.IntervalMap.Strict ( IntervalMap )
import qualified Data.IntervalMap.Strict as IM
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

type Handlers = IntervalMap Int ExceptionRange
handlersFor :: Handlers -> Int -> [ExceptionRange]
handlersFor h = map snd . IM.containing h

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
    (preds, succs) = buildPredecessors ivec handlers bvec bmapVec blockEnds
    (bvec, bnumMap) = splitIntoBlocks ivec handlers
    bmapVec = buildInstructionBlockMap bnumMap
    blockEnds = M.foldrWithKey collectEnds IS.empty bnumMap
    collectEnds (_, e) _ = IS.insert e
    handlers = foldr addHandler IM.empty ers
    addHandler r m =
      let s = fromIntegral $ erOffset r
          e = s + fromIntegral (erCount r)
      in IM.insert (IM.ClosedInterval s e) r m

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
                  -> Handlers
                  -> Vector (BlockNumber, Vector Instruction)
                  -> Vector BlockNumber
                  -> IntSet
                  -> (Vector [BlockNumber], Vector [BlockNumber])
buildPredecessors ivec handlers bvec bmap blockEnds =
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
      | not (isTerminator ivec handlers ix inst) &&
        IS.member ix blockEnds &&
        ix < V.length ivec - 1 =
        let target = ix + 1
            Just termBlock = bmap V.!? ix
            Just targetBlock = bmap V.!? target
        in (M.insertWith S.union targetBlock (S.singleton termBlock) pm,
            M.insertWith S.union termBlock (S.singleton targetBlock) sm)
      | otherwise =
        case terminatorAbsoluteTargets ivec handlers ix inst of
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
                -> Handlers
                -> (Vector (BlockNumber, Vector Instruction), Map (Int, Int) BlockNumber)
splitIntoBlocks ivec handlers = (V.indexed (V.fromList (reverse blocks)), blockRanges)
  where
    (blocks, blockRanges, _) = V.ifoldl' splitInstrs ([], M.empty, blockBeginnings) ivec
    blockBeginnings = V.ifoldl' (addTargetIndex ivec handlers) (IS.singleton 0) ivec
    splitInstrs :: ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
                   -> Int
                   -> Instruction
                   -> ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
    splitInstrs acc@(bs, ranges, blockStarts) ix inst
      | isTerminator ivec handlers ix inst || (ix + 1) `IS.member` blockStarts =
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
addTargetIndex :: Vector Instruction -> Handlers -> IntSet -> Int -> Instruction -> IntSet
addTargetIndex ivec handlers acc ix inst =
  case terminatorAbsoluteTargets ivec handlers ix inst of
    Nothing -> acc
    Just targets -> L.foldl' (flip IS.insert) acc targets

-- | Find the absolute target indices (into the instruction vector) for each
-- block terminator instruction.  The conditional branches have explicit
-- targets, but can also allow execution to fall through.
--
-- FIXME: Invoke *can* be a terminator if it is in a try block.  We do
-- need to know that here, technically.  Actually, any invoke or instruction
-- touching a reference can get an edge to an exception handler...
terminatorAbsoluteTargets :: Vector Instruction -> Handlers -> Int -> Instruction -> Maybe [Int]
terminatorAbsoluteTargets ivec handlers ix inst =
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
    -- Throw is always a terminator.  It may or may not have targets
    -- within the function.  We can also refine these if we *know* the
    -- concrete type of the exception thrown.
    Throw _ -> Just $ relevantHandlersInScope handlers ix inst
    -- For all other instructions, we need to do a more thorough lookup.
    -- See Note [Exceptional Control Flow] for details.
    _ ->
      case relevantHandlersInScope handlers ix inst of
        [] -> Nothing
        hs -> Just $ S.toList $ S.fromList hs

-- | Traverse handlers that are in scope from innermost to outermost.
-- If any matches the exception type thrown by this instruction, take
-- that as a handler for the exception.
relevantHandlersInScope :: Handlers -> Int -> Instruction -> [Int]
relevantHandlersInScope handlers ix inst =
  case relevantExceptions of
    NoExceptions -> []
    -- Note, this could be improved.  There might be some degree of
    -- subsumption between nested try/catch blocks.
    AllHandlers -> concatMap allTargetsIn hs
    SomeHandlers exns -> map fromIntegral $ concatMap closestHandler exns
  where
    closestHandler exns = foldr (matchingHandlerFor exns) [] hs
    matchingHandlerFor exns h acc =
      case L.find ((`elem` exns) . fst) (erCatch h) of
        Nothing -> maybe acc (:acc) (erCatchAll h)
        Just (_, off) -> maybe (off : acc) (:off:acc) (erCatchAll h)
    allTargetsIn r =
      let allHs = map snd (erCatch r)
      in map fromIntegral $ maybe allHs (:allHs) (erCatchAll r)
    hs = handlersFor handlers ix
    -- For this instruction, relevantExceptions are the types of exceptions
    -- that this instruction can generate.
    relevantExceptions = case inst of
      IBinop Div _ _ _ _ -> SomeHandlers [divZero]
      IBinop Rem _ _ _ _ -> SomeHandlers [divZero]
      IBinopAssign Div _ _ _ -> SomeHandlers [divZero]
      IBinopAssign Rem _ _ _ -> SomeHandlers [divZero]
      BinopLit8 Div _ _ _ -> SomeHandlers [divZero]
      BinopLit8 Rem _ _ _ -> SomeHandlers [divZero]
      BinopLit16 Div _ _ _ -> SomeHandlers [divZero]
      BinopLit16 Rem _ _ _ -> SomeHandlers [divZero]
      Invoke _ _ _ _ -> AllHandlers
      InstanceFieldOp _ _ _ _ -> SomeHandlers [nullPtr]
      ArrayOp (Get _) _ _ _ -> SomeHandlers [anyArray, nullPtr]
      ArrayOp (Put _) _ _ _ -> SomeHandlers [anyArray, arrayWrite, nullPtr]
      -- with a CHA, we could know precisely in many cases... otherwise all
      Throw _ -> AllHandlers
      FillArrayData _ _ -> SomeHandlers [nullPtr]
      NewArray _ _ _ -> SomeHandlers [newArray]
      ArrayLength _ _ -> SomeHandlers [nullPtr]
      CheckCast _ _ -> SomeHandlers [classCast]
      MonitorEnter _ -> SomeHandlers [nullPtr]
      MonitorExit _ -> SomeHandlers [nullPtr]
      _ -> NoExceptions
    -- Note that each of these lists is explicitly sorted with the
    -- most specific error types first.  Matching must be performed
    -- that way to find the handler that will actually be executed.
    -- Order within the HandlerType/SomeHandlers constructor is not
    -- important.
    anyRuntime = [ "Ljava.lang.RuntimeException;"
                 , "Ljava.lang.Exception;"
                 , "Ljava.lang.Throwable;"
                 ]
    divZero = "Ljava.lang.ArithmeticException;" : anyRuntime
    nullPtr = "Ljava.lang.NullPointerException;" : anyRuntime
    anyArray = "Ljava.lang.ArrayIndexOutOfBoundsException;" : "Ljava.lang.IndexOutOfBoundsException;" : anyRuntime
    arrayWrite = "Ljava.lang.ArrayStoreException;" : anyRuntime
    newArray = "Ljava.lang.NegativeArraySizeException;" : anyRuntime
    classCast = "Ljava.lang.ClassCastException;" : anyRuntime

data HandlerType = AllHandlers
                 | NoExceptions
                 | SomeHandlers [[BS.ByteString]]

isTerminator :: Vector Instruction -> Handlers -> Int -> Instruction -> Bool
isTerminator ivec handlers ix inst =
  isJust $ terminatorAbsoluteTargets ivec handlers ix inst

{- Note [Exceptional Control Flow]

1) Discover try nesting

2) Write a function, nearestHandler :: Int -> Maybe ExceptionRange

   Returns Nothing if the instruction is not guarded.  A variant of
   this would be to just note the active handler chain at every
   instruction.  That is probably better

3) If the nearest handler can handle a NullPointerException, each
instruction that can dereference a pointer gets an edge to the NPE
handler.  Similarly for the IndexOutOfBounds exceptions.  ArithmeticException after division...

Invokes can transfer execution to any type of handler.  We might be
able to do a respectable job of narrowing things down by examining the
throws clause of a method and routing more precise handlers later.

Once execution is in a catch block, everything should be fine.

-}
