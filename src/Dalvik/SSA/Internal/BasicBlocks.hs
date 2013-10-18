{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Split a stream of raw Dalvik 'Instruction's into basic blocks.
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
-- reasonable way.  This module supports:
--
--  * Getting a list of basic blocks.
--
--  * Querying the control flow graph (with efficient access to predecessors and successors).
--
--  * Figuring out which basic block an 'Instruction' came from.
--
--  * Resolving relative 'Instruction' offsets into absolute indices into the 'Instruction' stream.
--
--  * Determining what types of exceptions are handled in a basic block if that block is a catch handler.
--
-- Note that the construction of the control flow graph eliminates
-- some forms of dead code, notably exception handlers that cannot be
-- reached.
module Dalvik.SSA.Internal.BasicBlocks (
  -- * Types
  BasicBlocks,
  BlockNumber,
  TypeName,
  ExceptionRange(..),
  -- * Primary entry point
  findBasicBlocks,
  -- * Block-oriented queries
  basicBlockForInstruction,
  basicBlockPredecessors,
  basicBlockSuccessors,
  basicBlocksAsList,
  basicBlockHandlesException,
  JumpCondition(..),
  basicBlockBranchTargets,
  -- * Instruction-oriented queries
  instructionBlockNumber,
  instructionEndsBlock,
  instructionAtRawOffsetFrom
  ) where

import Control.Arrow ( second )
import qualified Data.ByteString as BS
import Data.Int ( Int64 )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.IntervalMap.Strict ( IntervalMap )
import qualified Data.IntervalMap.Strict as IM
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, isJust )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word ( Word32, Word16 )

import Dalvik.ClassName
import Dalvik.Instruction

-- | Types of Dalvik Type names
type TypeName = BS.ByteString

-- | Descriptions of exception handlers within a method.  Each of the
-- Word32 fields is an offset into the instruction stream (0-indexed).
-- @erOffset@ is the first @ushort@ in the instruction stream covered
-- by the try block. @erCount@ is the number of @ushort@s in the
-- instruction stream after the start covered by the try block.  The
-- other fields describe handlers.
data ExceptionRange =
  ExceptionRange { erOffset :: Word32
                 , erCount :: Word16
                 , erCatch :: [(BS.ByteString, Word32)]
                 , erCatchAll :: Maybe Word32
                 }
  deriving (Eq, Ord, Show)

-- | Unique (within a method) basic block identifiers
type BlockNumber = Int

-- | A simple type alias for dealing with these low-level BasicBlocks.
-- The first element is the number of the block (starts from zero).
-- The second element is the index (into the instruction stream) of
-- the first instruction in the BasicBlock.  The last element are the
-- instructions comprising the BasicBlock.
--
-- We store the index of the first instruction so that we can recover
-- the index of each instruction in the block.  We can't reconstruct
-- it without some stored information because dead code could be
-- eliminated from the middle of the instruction stream, leaving gaps
-- that we cannot account for otherwise.
type BlockDescriptor = (BlockNumber, Int, Vector Instruction)

-- | An opaque abstraction of basic blocks for the low-level Dalvik IR (Dalvik.Instruction)
data BasicBlocks =
  BasicBlocks { bbBlocks :: Vector (BlockNumber, Int, Vector Instruction)
                -- ^ The actual Basic Blocks.  These are *slices* out
                -- of the original Instruction vector.
              , bbFromInstruction :: IntervalMap Int BlockNumber
                -- ^ One entry per instruction, the reverse mapping
              , bbPredecessors :: Vector [BlockNumber]
                -- ^ One entry per basic block
              , bbSuccessors :: Vector [BlockNumber]
                -- ^ Again, one entry per basic block
              , bbBlockEnds :: IntSet
                -- ^ Ends of all block ranges.  We need these because
                -- some blocks end on implicit fallthrough on
                -- instructions that are *not* terminators.
              , bbBlockExceptionTypes :: Map BlockNumber BS.ByteString
                -- ^ For each basic block that begins a catch block,
                -- record the type of the exception handled by that
                -- catch block.  This is required to translate the
                -- move-exception instruction into SSA form.
              , bbBranchTargets :: Map BlockNumber [(JumpCondition, BlockNumber)]
                -- ^ Record the targets of the explicit terminator
                -- instructions in each block.  We need these to
                -- translate terminator statements, generally.
              , bbEnv :: BBEnv
              }
  deriving (Eq, Ord, Show)

-- | Given the index of an Instruction in the instruction vector and a
-- raw Dalvik offset, compute the instruction referenced by the
-- relative offset.  Raw offsets are in units of @ushort@, which do
-- **not** correspond one-to-one with instructions.
instructionAtRawOffsetFrom :: (Integral a) => BasicBlocks -> Int -> a -> Maybe Instruction
instructionAtRawOffsetFrom bbs ix offset =
  envInstVec (bbEnv bbs) V.!? targetIndex
  where
    targetIndex = resolveOffsetFrom (bbEnv bbs) ix (fromIntegral offset)

-- | Find the targets of the terminator (or fallthrough) instruction
-- in the named basic block.  Each target is tagged with the condition
-- under which that branch is taken.
basicBlockBranchTargets :: BasicBlocks -> BlockNumber -> [(JumpCondition, BlockNumber)]
basicBlockBranchTargets bbs bnum =
  fromMaybe errMsg $ M.lookup bnum (bbBranchTargets bbs)
  where
    errMsg = error ("Missing branch target for block " ++ show bnum)

-- | Determine what type, if any, is handled in the named basic block
basicBlockHandlesException :: BasicBlocks -> BlockNumber -> Maybe BS.ByteString
basicBlockHandlesException bbs bnum = M.lookup bnum (bbBlockExceptionTypes bbs)

-- | Extract the basic blocks into a more directly-usable form.
basicBlocksAsList :: BasicBlocks -> [(BlockNumber, Int, Vector Instruction)]
basicBlocksAsList = V.toList . bbBlocks

type Handlers = IntervalMap Int ExceptionRange
handlersFor :: Handlers -> Int -> [ExceptionRange]
handlersFor h = map snd . IM.containing h

data BBEnv =
  BBEnv { envInstVec :: Vector Instruction
        , envShortOffset :: Vector Int
          -- ^ This vector has one entry per Instruction in the
          -- instruction vector.  The entry is the original offset
          -- into the ushort array that the instructions were decoded
          -- from for that instruction.
        , envShortToInstIndex :: Map Int Int
          -- ^ Map each instruction's 'short offset' (position in the
          -- original data stream) to the Instruction.  Given the
          -- absolute position ('short offset') of a branch
          -- Instruction, retrieve the index into 'bbInstVec' of the
          -- target Instruction by looking it up here.
        , envExceptionHandlers :: Handlers
        }
  deriving (Eq, Ord, Show)

makeEnv :: Vector Instruction -> [ExceptionRange] -> BBEnv
makeEnv ivec ers =
  env0 { envExceptionHandlers = foldr addHandler IM.empty ers }
  where
    env0 = BBEnv { envInstVec = ivec
                 , envShortOffset = shortOffsets
                 , envShortToInstIndex = shortIndex
                 , envExceptionHandlers = IM.empty
                 }
    shortIndex = V.ifoldl' revMap M.empty shortOffsets
    shortOffsets = V.prescanl' instructionUnits 0 ivec
    instructionUnits acc inst = acc + insnUnitCount inst
    revMap acc ix off = M.insert off ix acc
    addHandler r m =
      let s = resolveOffsetFrom env0 0 $ fromIntegral $ erOffset r
          rest = V.drop s ivec
          e = s + V.ifoldr findLastCoveredIndex id rest (fromIntegral (erCount r))
      in IM.insert (IM.ClosedInterval s e) r m
    findLastCoveredIndex ix inst rest shortsLeft
      | shortsLeft <= 0 = error "Ran past the end of a function trying to find handler coverage"
      | shortsLeft == insnUnitCount inst = ix
      | otherwise = rest (shortsLeft - insnUnitCount inst)

resolveOffsetFrom :: BBEnv
                  -> Int -- ^ Index of the branch instruction
                  -> Int -- ^ Offset
                  -> Int -- ^ Target instruction index
resolveOffsetFrom env src off
  | Just srcShortOff <- envShortOffset env V.!? src =
    fromMaybe (errMsg srcShortOff) $ M.lookup (srcShortOff + off) (envShortToInstIndex env)
  | otherwise = error ("Missing a short offset for source instruction at index " ++ show src)
  where
    errMsg srcShortOff = error ("Missing instruction offset for target instruction at short index " ++ show (srcShortOff + off))

-- | Examine the instruction stream and break it into basic blocks.
-- This includes control flow information (block predecessors and
-- successors).  The 'BasicBlocks' object is opaque and can be queried
-- with the other functions exported from this module.
findBasicBlocks :: Vector Instruction -> [ExceptionRange] -> BasicBlocks
findBasicBlocks ivec ers =
  BasicBlocks { bbBlocks = bvec
              , bbFromInstruction = bnumMap
              , bbPredecessors = preds
              , bbSuccessors = succs
              , bbBlockEnds = blockEnds
              , bbBlockExceptionTypes =
                exceptionBlockTypes (resolveOffsetFrom env 0) bnumMap ers
              , bbBranchTargets = btargets
              , bbEnv = env
              }
  where
    env = makeEnv ivec ers
    (bvec, bnumMap) = splitIntoBlocks env
    blockEnds = IM.foldrWithKey collectEnds IS.empty bnumMap
    collectEnds i _ = let (IM.ClosedInterval _ e) = i in IS.insert e
    (preds, succs, btargets) = buildCFG env bvec bnumMap blockEnds


exceptionBlockTypes :: (Int -> Int)
                       -> IntervalMap Int BlockNumber
                       -> [ExceptionRange]
                       -> Map BlockNumber BS.ByteString
exceptionBlockTypes toInstIdx bnumMap =
  L.foldl' addBlockHandlerTypes M.empty
  where
    addBlockHandlerTypes m er = L.foldl' addHandlerType m (erCatch er)
    addHandlerType m (name, offset) =
      -- If the index isn't found in bnumMap, that means the handler
      -- is dead code (no exceptions can actually reach it at
      -- runtime).
      let instIdx = toInstIdx (fromIntegral offset)
      in case IM.containing bnumMap instIdx of
        [(_, bnum)] -> M.insert bnum name m
        _ -> m

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
instructionBlockNumber bbs ix =
  case IM.containing (bbFromInstruction bbs) ix of
    [(_, bnum)] -> Just bnum
    _ -> Nothing

-- | Get the basic block ID for the instruction at the given index.
basicBlockForInstruction :: BasicBlocks -> Int -> BlockNumber
basicBlockForInstruction bbs ix =
  case IM.containing (bbFromInstruction bbs) ix of
    [(_, bnum)] -> bnum
    _ -> error ("No BasicBlock for Instruction at index " ++ show ix)

basicBlockPredecessors :: BasicBlocks -> BlockNumber -> [BlockNumber]
basicBlockPredecessors bbs bid =
  fromMaybe errMsg $ bbPredecessors bbs V.!? bid
  where
    errMsg = error ("No predecessor entry for BasicBlock " ++ show bid)

basicBlockSuccessors :: BasicBlocks -> BlockNumber -> [BlockNumber]
basicBlockSuccessors bbs bid =
  fromMaybe errMsg $ bbSuccessors bbs V.!? bid
  where
    errMsg = error ("No successor entry for BasicBlock " ++ show bid)

-- | Iterate over the block list and construct a control flow graph.
--
-- The control flow graph is encoded simply as just vectors of
-- predecessors and successors.  Each of these vectors has one entry
-- for each basic block: a list of predecessors or successors.
--
-- While building the control flow graph, we also record the targets
-- of each branch instruction.  Targets are basic block numbers.  This
-- information is easiest to gather while constructing the CFG, so we
-- do so here even though it is not exactly part of the CFG.
buildCFG :: BBEnv
            -> Vector BlockDescriptor
            -> IntervalMap Int BlockNumber
            -> IntSet
            -> (Vector [BlockNumber], Vector [BlockNumber], Map BlockNumber [(JumpCondition, BlockNumber)])
buildCFG env bvec bmap blockEnds =
  (V.generate (V.length bvec) getPreds,
   V.generate (V.length bvec) getSuccs,
   branchTargets)
  where
    ivec = envInstVec env
    getPreds ix = S.toList $ fromMaybe S.empty $ M.lookup ix predMap
    getSuccs ix = S.toList $ fromMaybe S.empty $ M.lookup ix succMap
    (predMap, succMap, branchTargets) =
      V.ifoldl' addBlockInfo (M.empty, M.empty, M.empty) ivec
    addBlockInfo m@(pm, sm, bts) ix inst
      -- @inst@ is a fallthrough instruction that implicitly ends a
      -- block, so we need to make an entry for it.  We don't need to
      -- update the branchTargets map in this branch because we only
      -- need to record those for explicit branches.  Fallthrough targets
      -- are always obvious.
      | not (isTerminator env ix inst) && IS.member ix blockEnds =
        let target = ix + 1
            termBlock = blockForInstruction bmap ix
            targetBlock = blockForInstruction bmap target
        in (M.insertWith S.union targetBlock (S.singleton termBlock) pm,
            M.insertWith S.union termBlock (S.singleton targetBlock) sm,
            bts)
      -- If this is a terminator and there is no block mapping for it,
      -- that means that the terminator instruction is in dead code
      -- (so we can ignore it).  Otherwise, record CFG information and
      -- the branch targets.
      | Just targets <- terminatorAbsoluteTargets env ix inst
      , [(_, termBlock)] <- IM.containing bmap ix =
        let instIndexToBlockNum = second (blockForInstruction bmap)
            targetBlocks = map instIndexToBlockNum targets
            addSuccsPreds (p, s, b) (condition, targetBlock) =
              (M.insertWith S.union targetBlock (S.singleton termBlock) p,
               M.insertWith S.union termBlock (S.singleton targetBlock) s,
               M.insertWith (++) termBlock [(condition, targetBlock)] b)
        in L.foldl' addSuccsPreds m targetBlocks
      | otherwise = m

blockForInstruction :: IntervalMap Int BlockNumber -> Int -> BlockNumber
blockForInstruction m ix =
  case IM.containing m ix of
    [(_, b)] -> b
    _ -> error ("No BasicBlock for instruction at index " ++ show ix)

-- Splitting into blocks

-- Scan through the instruction vector and end a block if either: 1) @ix@ is a terminator
-- or 2) @ix + 1@ starts a block.

-- | Scan through the instruction stream and construct basic blocks.
-- Basic blocks end if the current instruction is a terminator or if
-- the next instruction is the target of a branch (in which case the
-- current instruction implicitly falls through to the new block).
--
-- The algorithm is as follows:
--
--  1) Figure out where each basic block begins with one pass over the
--  instruction stream.  Blocks begin at each terminator instruction
--  target (so we can't know all targets without examining all
--  terminator instructions).
--
--  2) Fold over the instructions in a separate pass, creating basic
--  blocks whenever we either hit a terminator instruction OR if the
--  next instruction begins a new block (by virtue of being a branch
--  target).  Basic blocks are just slices out of the original
--  instruction stream.  The bounds of each basic block (indices into
--  the instruction stream) are stored in an IntervalMap so that we
--  can efficiently look up which block a given instruction is in.
--  Since the IntervalMap contains only non-overlapping regions,
--  lookups are @log(N)@ where @N@ is the number of basic blocks.
--
-- This function takes care of dead code elimination (see the first
-- guard in @splitInstrs@).
--
-- This function returns two values: the vector of BasicBlocks that
-- were constructed and an interval map.  The interval map records,
-- for ranges of instructions, which basic block they belong to.  This
-- is a reverse mapping from instruction index to block number.  There
-- can be gaps in the interval map (due to dead code elimination).
-- The intervals are non-overlapping since blocks are not nested.
splitIntoBlocks :: BBEnv -> (Vector BlockDescriptor, IntervalMap Int BlockNumber)
splitIntoBlocks env = (V.fromList (reverse blocks), blockRanges)
  where
    ivec = envInstVec env
    (blocks, blockRanges, _, _) = V.ifoldl' splitInstrs ([], IM.empty, blockBeginnings, 0) ivec
    blockBeginnings = V.ifoldl' (addTargetIndex env) (IS.singleton 0) ivec
    splitInstrs :: ([BlockDescriptor], IntervalMap Int BlockNumber, IntSet, Int)
                   -> Int
                   -> Instruction
                   -> ([BlockDescriptor], IntervalMap Int BlockNumber, IntSet, Int)
    splitInstrs acc@(bs, ranges, blockStarts, bnum) ix inst
    -- If we have no more block starts OR if the next block start is
    -- after the current instruction, that means we are translating
    -- dead code and it doesn't need to be turned into a block.
      | IS.null blockStarts || blockStart > ix = acc
      | isTerminator env ix inst || (ix + 1) `IS.member` blockStarts =
        let len = ix - blockStart + 1
        in ((bnum, blockStart, V.slice blockStart len ivec) : bs,
            IM.insert (IM.ClosedInterval blockStart ix) bnum ranges,
            blockStarts',
            bnum + 1)
      | otherwise = acc
      where
        (blockStart, blockStarts') = IS.deleteFindMin blockStarts

-- | If the instruction is a jump, add all of its possible target
-- indices (absolute, based from 0) to the Set.  While data loss is
-- technically possible because of a conversion from 'Int32' to 'Int',
-- that isn't a problem in practice because we wouldn't have enough
-- RAM for an array big enough to suffer from the loss.
--
-- Note that conditional branch instructions actually have two
-- targets: the explicit one and the implicit fallthrough target.
-- The fallthrough needs to begin a new block, too.
addTargetIndex :: BBEnv -> IntSet -> Int -> Instruction -> IntSet
addTargetIndex env acc ix inst =
  case terminatorAbsoluteTargets env ix inst of
    Nothing -> acc
    Just targets -> L.foldl' (flip IS.insert) acc (map snd targets)

-- | These are tags describing the condition under which each branch
-- is taken.
data JumpCondition = Unconditional -- ^ Always taken
                   | Fallthrough   -- ^ A fallthrough (e.g., taken when a conditional branch test fails)
                   | SwitchCase Int64 -- ^ Taken if the switch value is equal to the included 'Int64'
                   | Conditional -- ^ Taken if a conditional branch test evaluates to True
                   | Exceptional -- ^ Taken if an exception is thrown
                   deriving (Eq, Ord, Show)

-- | Find the absolute target indices (into the instruction vector)
-- for each block terminator instruction.  The conditional branches
-- have explicit targets, but can also allow execution to fall
-- through.
--
-- This function also accounts for exceptional control flow.
-- Primitive instructions that can raise exceptions (due to null
-- pointers, out of memory conditions, arithmetic exceptions, etc)
-- become terminator instructions if they are guarded by a try/catch
-- block that is capable of handling their types of exceptions.
-- Invoke instructions are terminators if there are *any* exception
-- handlers in scope.  Note that we can refine that model and make the
-- CFG more precise by looking at some more dalvik metadata that
-- records what checked exception types each method can throw.
terminatorAbsoluteTargets :: BBEnv -> Int -> Instruction -> Maybe [(JumpCondition, Int)]
terminatorAbsoluteTargets env ix inst =
  case inst of
    Goto i8 -> Just [(Unconditional, resolveOffsetFrom env ix (fromIntegral i8))]
    Goto16 i16 -> Just [(Unconditional, resolveOffsetFrom env ix (fromIntegral i16))]
    Goto32 i32 -> Just [(Unconditional, resolveOffsetFrom env ix (fromIntegral i32))]
    PackedSwitch _ tableOff ->
      let tableVecOff = resolveOffsetFrom env ix (fromIntegral tableOff)
          ivec = envInstVec env
          Just (PackedSwitchData startVal offs) = ivec V.!? tableVecOff
          indexedOffsets = zip [startVal..] offs
          ft = (Fallthrough, ix + 1)
          cases = [ (SwitchCase (fromIntegral switchVal), resolveOffsetFrom env ix (fromIntegral o))
                  | (switchVal, o) <- indexedOffsets
                  ]
      in Just (ft : cases)
    SparseSwitch _ tableOff ->
      let tableVecOff = resolveOffsetFrom env ix (fromIntegral tableOff)
          ivec = envInstVec env
          Just (SparseSwitchData vals offs) = ivec V.!? tableVecOff
          indexedOffsets = zip vals offs
          ft = (Fallthrough, ix + 1)
          cases = [ (SwitchCase (fromIntegral switchVal), resolveOffsetFrom env ix (fromIntegral o))
                  | (switchVal, o) <- indexedOffsets
                  ]
      in Just (ft : cases)
    If _ _ _ off -> Just [ (Fallthrough, ix + 1)
                         , (Conditional, resolveOffsetFrom env ix (fromIntegral off))
                         ]
    IfZero _ _ off -> Just [ (Fallthrough, ix + 1)
                           , (Conditional, resolveOffsetFrom env ix (fromIntegral off))
                           ]
    ReturnVoid -> Just []
    Return _ _ -> Just []
    -- Throw is always a terminator.  It may or may not have targets
    -- within the function.  We can also refine these if we *know* the
    -- concrete type of the exception thrown.
    Throw _ -> Just $ map (Exceptional,) $ relevantHandlersInScope env ix inst
    -- For all other instructions, we need to do a more thorough lookup.
    -- See Note [Exceptional Control Flow] for details.
    --
    -- If there are handlers in scope, we don't always branch to them
    -- - we can just fall through if there is no exception.  Include
    -- the fallthrough instruction as a target.
    _ ->
      case relevantHandlersInScope env ix inst of
        [] -> Nothing
        hs -> Just $ (Fallthrough, ix+1) : map (Exceptional,) (S.toList (S.fromList hs))

-- | Traverse handlers that are in scope from innermost to outermost.
-- If any matches the exception type thrown by this instruction, take
-- that as a handler for the exception.
--
-- See the environment, but exception handlers are stored in an
-- interval map (the interval is the range of instructions covered by
-- a try block, and the value payload is the list of exception
-- handlers handled by that block).
relevantHandlersInScope :: BBEnv -> Int -> Instruction -> [Int]
relevantHandlersInScope env ix inst =
  case relevantExceptions of
    NoExceptions -> []
    -- Note, this could be improved.  There might be some degree of
    -- subsumption between nested try/catch blocks.
    AllHandlers -> concatMap allTargetsIn hs
    -- Each element of @exns@ is one primitive exception.  These are
    -- lists of the exception and its superclasses.  A handler for a
    -- superclass can also handle the exception at element 0.
    --
    -- We want to find a handler for *each* possible exception.  We
    -- only need to take the first we encounter.  Rethrows are handled
    -- by the logic for the throw instruction.
    SomeHandlers exns -> map fromIntegral $ concatMap closestHandler exns
  where
    handlers = envExceptionHandlers env
    -- This is an early-terminating fold.  It has to collect all
    -- 'finally' blocks from non-matching handlers, but it can stop
    -- once it finds the first actual handler (and associated
    -- finally).
    closestHandler exns = foldr (matchingHandlerFor (map renderClassName exns)) id hs []
    addCatchAll h acc =
      case erCatchAll h of
        Nothing -> acc
        Just off -> resolveOffsetFrom env 0 (fromIntegral off) : acc
    matchingHandlerFor exns h rest acc =
      case L.find ((`elem` exns) . fst) (erCatch h) of
        Nothing -> rest $ addCatchAll h acc
        Just (_, off) ->
          let acc' = resolveOffsetFrom env 0 (fromIntegral off) : acc
          in addCatchAll h acc'
    allTargetsIn r =
      let allHs = map snd (erCatch r)
      in map (fromIntegral . resolveOffsetFrom env 0 . fromIntegral) $ maybe allHs (:allHs) (erCatchAll r)
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
      -- If we had an iterative or interleaved CFG construction, we
      -- could prove that some instance field operations are on
      -- 'this', which is never NULL.  Furthermore, we only need the
      -- NPE edge after the first dereference of each reference (after
      -- that, we know that it must not be NULL if control reaches, at
      -- least within a basic block)
      InstanceFieldOp _ _ _ _ -> SomeHandlers [nullPtr]
      ArrayOp (Get _) _ _ _ -> SomeHandlers [anyArray, nullPtr]
      ArrayOp (Put _) _ _ _ -> SomeHandlers [anyArray, arrayWrite, nullPtr]
      -- with a CHA, we could know precisely in many cases... otherwise all
      Throw _ -> AllHandlers
      FillArrayData _ _ -> SomeHandlers [nullPtr]
      FilledNewArray _ _ -> SomeHandlers [oom]
      FilledNewArrayRange _ _ -> SomeHandlers [oom]
      NewArray _ _ _ -> SomeHandlers [newArray, oom]
      ArrayLength _ _ -> SomeHandlers [nullPtr]
      NewInstance _ _ -> SomeHandlers [oom]
      CheckCast _ _ -> SomeHandlers [classCast]
      MonitorEnter _ -> SomeHandlers [nullPtr]
      MonitorExit _ -> SomeHandlers [nullPtr]
      _ -> NoExceptions
    -- Note that each of these lists is explicitly sorted with the
    -- most specific error types first.  Matching must be performed
    -- that way to find the handler that will actually be executed.
    -- Order within the HandlerType/SomeHandlers constructor is not
    -- important.
    anyRuntime = [ jl "RuntimeException"
                 , jl "Exception"
                 , jl "Throwable"
                 ]
    anyVMError = [ jl "VirtualMachineError"
                 , jl "Error"
                 , jl "Throwable"
                 ]
    divZero = jl "ArithmeticException" : anyRuntime
    nullPtr = jl "NullPointerException" : anyRuntime
    anyArray = jl "ArrayIndexOutOfBoundsException" : jl "IndexOutOfBoundsException" : anyRuntime
    arrayWrite = jl "ArrayStoreException" : anyRuntime
    newArray = jl "NegativeArraySizeException" : anyRuntime
    classCast = jl "ClassCastException" : anyRuntime
    oom = jl "OutOfMemoryError" : anyVMError

-- | Add a java/lang namespace prefix to the given exception name.
jl :: BS.ByteString -> ClassName
jl = qualifiedClassName ["java", "lang"]

data HandlerType = AllHandlers
                 | NoExceptions
                 | SomeHandlers [[ClassName]]

isTerminator :: BBEnv -> Int -> Instruction -> Bool
isTerminator env ix inst =
  isJust $ terminatorAbsoluteTargets env ix inst

