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
  basicBlockHandlesException,
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

import Dalvik.ClassHierarchy
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
              , bbBlockExceptionTypes :: Map BlockNumber BS.ByteString
              }
  deriving (Eq, Ord, Show)

-- | Determine what type, if any, is handled in the named basic block
basicBlockHandlesException :: BasicBlocks -> BlockNumber -> Maybe BS.ByteString
basicBlockHandlesException bbs bnum = M.lookup bnum (bbBlockExceptionTypes bbs)

-- | Extract the basic blocks into a more directly-usable form.
basicBlocksAsList :: BasicBlocks -> [(BlockNumber, Vector Instruction)]
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
          e = resolveOffsetFrom env0 s $ fromIntegral $ erCount r -- - 1
      in IM.insert (IM.ClosedInterval s e) r m

resolveOffsetFrom :: BBEnv
                  -> Int -- ^ Index of the branch instruction
                  -> Int -- ^ Offset
                  -> Int -- ^ Target instruction index
resolveOffsetFrom env src off = ix
  where
    Just srcShortOff = envShortOffset env V.!? src
    Just ix = M.lookup (srcShortOff + off) (envShortToInstIndex env)

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
              , bbBlockExceptionTypes =
                exceptionBlockTypes (resolveOffsetFrom env 0) bmapVec ers
              }
  where
    env = makeEnv ivec ers
    (preds, succs) = buildPredecessors env bvec bmapVec blockEnds
    (bvec, bnumMap) = splitIntoBlocks env
    bmapVec = buildInstructionBlockMap bnumMap
    blockEnds = M.foldrWithKey collectEnds IS.empty bnumMap
    collectEnds (_, e) _ = IS.insert e

exceptionBlockTypes :: (Int -> Int)
                       -> Vector BlockNumber
                       -> [ExceptionRange]
                       -> Map BlockNumber BS.ByteString
exceptionBlockTypes toInstIdx bnumMap =
  L.foldl' addBlockHandlerTypes M.empty
  where
    addBlockHandlerTypes m er = L.foldl' addHandlerType m (erCatch er)
    addHandlerType m (name, offset) =
      let instIdx = toInstIdx (fromIntegral offset)
          Just bnum = bnumMap V.!? instIdx
      in M.insert bnum name m

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
buildPredecessors :: BBEnv
                  -> Vector (BlockNumber, Vector Instruction)
                  -> Vector BlockNumber
                  -> IntSet
                  -> (Vector [BlockNumber], Vector [BlockNumber])
buildPredecessors env bvec bmap blockEnds =
  (V.generate (V.length bvec) getPreds,
   V.generate (V.length bvec) getSuccs)
  where
    ivec = envInstVec env
    getPreds ix = S.toList $ fromMaybe S.empty $ M.lookup ix predMap
    getSuccs ix = S.toList $ fromMaybe S.empty $ M.lookup ix succMap
    (predMap, succMap) = V.ifoldl' addTermSuccs (M.empty, M.empty) ivec
    addTermSuccs m@(pm, sm) ix inst
      -- @inst@ is a fallthrough instruction that implicitly ends a block,
      -- so we need to make an entry for it.  We don't make an entry for the
      -- last instruction in the function.
      | not (isTerminator env ix inst) &&
        IS.member ix blockEnds &&
        ix < V.length ivec - 1 =
        let target = ix + 1
            Just termBlock = bmap V.!? ix
            Just targetBlock = bmap V.!? target
        in (M.insertWith S.union targetBlock (S.singleton termBlock) pm,
            M.insertWith S.union termBlock (S.singleton targetBlock) sm)
      | otherwise = fromMaybe m $ do
          targets <- terminatorAbsoluteTargets env ix inst
          termBlock <- bmap V.!? ix
          let targetBlocks = mapMaybe (bmap V.!?) targets
              addSuccsPreds (p, s) targetBlock = (M.insertWith S.union targetBlock (S.singleton termBlock) p,
                                                  M.insertWith S.union termBlock (S.singleton targetBlock) s)
          return $ L.foldl' addSuccsPreds m targetBlocks

buildInstructionBlockMap :: Map (Int, Int) BlockNumber -> Vector BlockNumber
buildInstructionBlockMap =
  V.fromList . map snd . L.sort . concatMap fromRange . M.toList
  where
    fromRange ((start, end), bnum) = zip [start..end] (repeat bnum)


-- Splitting into blocks

-- Scan through the instruction vector and end a block if either: 1) @ix@ is a terminator
-- or 2) @ix + 1@ starts a block.
splitIntoBlocks :: BBEnv
                -> (Vector (BlockNumber, Vector Instruction),
                    Map (Int, Int) BlockNumber)
splitIntoBlocks env = (V.indexed (V.fromList (reverse blocks)), blockRanges)
  where
    ivec = envInstVec env
    (blocks, blockRanges, _) = V.ifoldl' splitInstrs ([], M.empty, blockBeginnings) ivec
    blockBeginnings = V.ifoldl' (addTargetIndex env) (IS.singleton 0) ivec
    splitInstrs :: ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
                   -> Int
                   -> Instruction
                   -> ([Vector Instruction], Map (Int, Int) BlockNumber, IntSet)
    splitInstrs acc@(bs, ranges, blockStarts) ix inst
    -- If we have no more block starts OR if the next block start is
    -- after the current instruction, that means we are translating
    -- dead code and it doesn't need to be turned into a block.
      | IS.null blockStarts || blockStart > ix = acc
      | isTerminator env ix inst || (ix + 1) `IS.member` blockStarts =
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
addTargetIndex :: BBEnv -> IntSet -> Int -> Instruction -> IntSet
addTargetIndex env acc ix inst =
  case terminatorAbsoluteTargets env ix inst of
    Nothing -> acc
    Just targets -> L.foldl' (flip IS.insert) acc targets

-- | Find the absolute target indices (into the instruction vector) for each
-- block terminator instruction.  The conditional branches have explicit
-- targets, but can also allow execution to fall through.
terminatorAbsoluteTargets :: BBEnv -> Int -> Instruction -> Maybe [Int]
terminatorAbsoluteTargets env ix inst =
  case inst of
    Goto i8 -> Just [resolveOffsetFrom env ix (fromIntegral i8)]
    Goto16 i16 -> Just [resolveOffsetFrom env ix (fromIntegral i16)]
    Goto32 i32 -> Just [resolveOffsetFrom env ix (fromIntegral i32)]
    PackedSwitch _ tableOff ->
      let tableVecOff = resolveOffsetFrom env ix (fromIntegral tableOff)
          ivec = envInstVec env
          Just (PackedSwitchData _ offs) = ivec V.!? tableVecOff
      in Just (ix + 1 : [resolveOffsetFrom env ix (fromIntegral o) | o <- offs])
    SparseSwitch _ tableOff ->
      let tableVecOff = resolveOffsetFrom env ix (fromIntegral tableOff)
          ivec = envInstVec env
          Just (SparseSwitchData _ offs) = ivec V.!? tableVecOff
      in Just (ix + 1 : [resolveOffsetFrom env ix (fromIntegral o) | o <- offs])
    If _ _ _ off -> Just [ix + 1, resolveOffsetFrom env ix (fromIntegral off)]
    IfZero _ _ off -> Just [ix + 1, resolveOffsetFrom env ix (fromIntegral off)]
    ReturnVoid -> Just []
    Return _ _ -> Just []
    -- Throw is always a terminator.  It may or may not have targets
    -- within the function.  We can also refine these if we *know* the
    -- concrete type of the exception thrown.
    Throw _ -> Just $ relevantHandlersInScope env ix inst
    -- For all other instructions, we need to do a more thorough lookup.
    -- See Note [Exceptional Control Flow] for details.
    --
    -- If there are handlers in scope, we don't always branch to them
    -- - we can just fall through if there is no exception.  Include
    -- the fallthrough instruction as a target.
    _ ->
      case relevantHandlersInScope env ix inst of
        [] -> Nothing
        hs -> Just $ S.toList $ S.fromList $ (ix+1) : hs

-- | Traverse handlers that are in scope from innermost to outermost.
-- If any matches the exception type thrown by this instruction, take
-- that as a handler for the exception.
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
