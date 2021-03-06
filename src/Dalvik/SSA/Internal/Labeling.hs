{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module implements SSA value numbering over the low-level Dalvik IR
--
-- The algorithm used is from Braun et al
-- (<http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf>).  The
-- labeling maps each operand of a low-level Dalvik instruction to
-- the SSA number of that operand.  Note that SSA numbers are only
-- required for *instructions*.  Constants are, well, constant, and
-- can be handled directly during translation.
--
-- Translation to the SSA IR requires knot tying.  Many instructions
-- can be ignored (moves, data pseudo-registers).  Instructions with a
-- destination register get assigned the number placed on their
-- destination (in fact, they create a new value by virtue of having
-- that destination).
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
--
-- > loop:
-- >   r4 <- phi(r3, r1)
-- >   r3 <- binop add r4 r2
-- >   br (r4 < 100) loop
module Dalvik.SSA.Internal.Labeling (
  -- * Data Types
  Label(..),
  Labeling(..),
  ExceptionRange(..),
  FromRegister(..),
  labelMethod,
  labelingInstructionAt,
  labelingPhiIncomingValues,
  filterWidePairs,
  -- * Testing
  prettyLabeling,
  generatedLabelsAreUnique
  ) where

import qualified Control.Monad.Catch as E
import Control.Monad ( filterM, forM_, liftM )
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.Function ( on )
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import qualified Data.List as L
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word ( Word8, Word16 )
import Text.PrettyPrint as PP
import Text.Printf

import Dalvik.Types as DT
import Dalvik.Instruction as DT
import Dalvik.SSA.Internal.BasicBlocks
import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Internal.RegisterAssignment

-- | Types of label.  Arguments and Phi nodes have special labels
-- carrying extra information.
data Label = SimpleLabel Int
           | PhiLabel BlockNumber [BlockNumber] Int
           | ArgumentLabel BS.ByteString Int
           deriving (Show)

labelNumber :: Label -> Int
labelNumber (SimpleLabel i) = i
labelNumber (PhiLabel _ _ i) = i
labelNumber (ArgumentLabel _ i) = i

instance Eq Label where
  (==) = (==) `on` labelNumber

instance Ord Label where
  compare = compare `on` labelNumber

-- | A labeling assigns an SSA number/Label to a register at *each*
-- 'Instruction'.
data Labeling =
  Labeling { labelingReadRegs :: Map Int (Map Word16 Label)
           , labelingWriteRegs :: Map Int Label
           , labelingPhis :: Map Label (Set Label)
             -- ^ The incoming values for each phi node (phi labels are the map keys)
           , labelingPhiSources :: Map Label (Map Label (Set BlockNumber))
             -- ^ Each key is (PhiLabel, IncomingValueLabel) and the
             -- value associated is the basic block that the incoming
             -- value came from.  We want to separate them out so that
             -- trivial phi detection is simple (and left up to the
             -- Set).  This map can safely have dead values, since it
             -- will only be read from at SSA translation time, and
             -- only relevant values will be queried.
           , labelingBlockPhis :: Map BlockNumber [Label]
             -- ^ For each basic block, note which phi labels belong
             -- to it.  This will be needed when we translate basic
             -- blocks to the SSA IR.
           , labelingBasicBlocks :: BasicBlocks
           , labelingInstructions :: Vector Instruction
           , labelingParameters :: [Label]
             -- ^ The argument labels allocated for this function
           }
  deriving (Eq, Ord, Show)

-- | Look up all of the incoming values (tagged with the basic block
-- that they came from) for a phi node label.  The basic block numbers
-- are stored separately to make trivial phi elimination simpler, so
-- we need to attach them to their labels here.
labelingPhiIncomingValues :: Labeling -> Label -> [(BlockNumber, Label)]
labelingPhiIncomingValues labeling phi = concatMap addBlockNumber (S.toList ivs)
  where
    Just ivs = M.lookup phi (labelingPhis labeling)
    addBlockNumber iv
      | Just m <- M.lookup phi (labelingPhiSources labeling)
      , Just tagged <- M.lookup iv m =
        zip (S.toList tagged) (repeat iv)
      | otherwise = error ("Missing labelingPhiSources for " ++ show phi)

-- | The mutable state we are modifying while labeling.  This mainly
-- models the register state and phi operands.
data LabelState =
  LabelState { currentDefinition :: Map Word16 (Map BlockNumber Label)
               -- ^ We track the current definition of each register at
               -- each block.
             , instructionLabels :: Map Int (Map Word16 Label)
               -- ^ This is separate from @currentDefinition@ because
               -- that field is mutated.
             , instructionResultLabels :: Map Int Label
               -- ^ Store the label of instructions that produce
               -- a result separately from the normal map.  This is only
               -- really required because of the compound instructions
               -- 'IBinopAssign' and 'FBinopAssign'
             , registersWithLabel :: Map Label (Set (Word16, BlockNumber))
               -- ^ A reverse mapping to let us know which registers
               -- (in which block) refer to a label in
               -- 'currentDefinition'.  We need this reverse map to
               -- efficiently update just those entries when
               -- eliminating phi nodes.
             , phiOperands :: Map Label (Set Label)
               -- ^ Operands of each phi node.  They are stored
               -- separately to make them easier to update (since they
               -- do need to be updated in several cases).
             , phiOperandSources :: Map Label (Map Label (Set BlockNumber))
               -- ^ Record where each phi operand came from (a block
               -- number).  Note that one operand could have come from
               -- more than one block (hence the 'Set').  This is only
               -- used when constructing the Labeling.
             , incompletePhis :: Map BlockNumber (Map Word16 Label)
               -- ^ These are the phi nodes in each block that cannot
               -- be resolved yet because there are unfilled
               -- predecessor blocks.  Unfilled predecessors could add
               -- definitions that affect the phi node by the time
               -- they are filled.  Once those blocks are filled, the
               -- incomplete phis will be filled in.
             , eliminatedPhis :: Map Label Label
               -- ^ If we have eliminated a phi node, track what
               -- references were replaced with.  This must be
               -- searched transitively.
             , valueUsers :: Map Label (Set Use)
               -- ^ Record the users of each value.  This index is
               -- necessary for phi simplification.  When a trivial
               -- phi node is found, all uses of that phi node need to
               -- be replaced by a simpler phi node or another value.
               -- These users will be used to modify @instructionLabels@.
             , filledBlocks :: IntSet
               -- ^ A block is filled if all of its instructions have been
               -- labelled.
             , sealedBlocks :: IntSet
               -- ^ A block is sealed if all of its predecessors are
               -- filled.  Phi node operands are only computed for
               -- sealed blocks (so that all operands are available).
             , labelCounter :: Int
               -- ^ A source of label unique IDs
             }

data Use = NormalUse Int Instruction Word16
         | PhiUse Label
         deriving (Eq, Ord, Show)

data LabelEnv =
  LabelEnv { envInstructionStream :: Vector Instruction
           , envBasicBlocks :: BasicBlocks
             -- ^ Information about basic blocks in the instruction
             -- stream
           , envRegisterAssignment :: Map Word16 Label
             -- ^ The map of argument registers to their labels
           }



-- | Create a new empty SSA labeling environment.  This includes
-- computing CFG information.
emptyEnv :: [(BS.ByteString, Word16)] -> [ExceptionRange] -> Vector Instruction -> LabelEnv
emptyEnv argRegs ers ivec =
  LabelEnv { envInstructionStream = ivec
           , envBasicBlocks = findBasicBlocks ivec ers
           , envRegisterAssignment =
             fst $ L.foldl' allocateArgLabel (M.empty, 0) argRegs
           }
  where
    allocateArgLabel (m, lno) (name, reg) =
      (M.insert reg (ArgumentLabel name lno) m, lno + 1)

-- | Create a new empty state.  This state is initialized to account
-- for method arguments, which occupy the last @length argRegs@
-- registers.  Wide arguments take two registers, but we only need to
-- worry about the first register in the pair (since the second is
-- never explicitly accessed).
emptyLabelState :: [(BS.ByteString, Word16)] -> LabelState
emptyLabelState argRegs =
  LabelState { currentDefinition = fst $ L.foldl' addArgDef (M.empty, 0) argRegs
             , instructionLabels = M.empty
             , instructionResultLabels = M.empty
             , registersWithLabel = M.empty
             , phiOperands = M.empty
             , phiOperandSources = M.empty
             , incompletePhis = M.empty
             , eliminatedPhis = M.empty
             , valueUsers = M.empty
             , filledBlocks = IS.empty
             , sealedBlocks = IS.empty
             , labelCounter = fromIntegral $ length argRegs
             }
  where
    addArgDef (m, lno) (name, reg) =
      let l = ArgumentLabel name lno
      in (M.insertWith M.union reg (M.singleton 0 l) m, lno + 1)

-- | Look up the low-level Dalvik instruction at the given index into
-- the original instruction stream.
labelingInstructionAt :: Labeling -> Int -> Maybe Instruction
labelingInstructionAt l = (labelingInstructions l V.!?)

-- | Compute SSA value labels for each register at each instruction in
-- the given method.  We also compute some extra information about
-- parameters and phi nodes that are required for a complete SSA
-- transformation.
--
-- See note [Overview] for details
labelMethod :: (E.MonadThrow m) => DT.DexFile -> DT.EncodedMethod -> m Labeling
labelMethod _ (DT.EncodedMethod mId _ Nothing) = E.throwM $ NoCodeForMethod mId
labelMethod dx em@(DT.EncodedMethod _ _ (Just codeItem)) = do
  insts <- DT.decodeInstructions (codeInsns codeItem)
  regMap <- methodRegisterAssignment dx em
  ers <- methodExceptionRanges dx em
  labelInstructions dx regMap ers insts

-- | Parse the try/catch description tables for this 'EncodedMethod'
-- from the DexFile.  The tables are reduced to summaries
-- ('ExceptionRange') that are easier to work with.
methodExceptionRanges :: (E.MonadThrow m) => DT.DexFile -> EncodedMethod -> m [ExceptionRange]
methodExceptionRanges _ (DT.EncodedMethod mId _ Nothing) = E.throwM $ NoCodeForMethod mId
methodExceptionRanges dx (DT.EncodedMethod _ _ (Just codeItem)) =
  mapM toExceptionRange (codeTryItems codeItem)
  where
    catches = V.fromList $ codeHandlers codeItem
    toExceptionRange tryItem = do
      let hOffset = fromIntegral $ tryHandlerOff tryItem
      case V.findIndex ((==hOffset) . chHandlerOff) catches of
        Nothing -> E.throwM $ NoHandlerAtOffset hOffset
        Just cix -> do
          let errMsg = error ("No catch handler entry for handler at offset " ++ show cix)
          let ch = fromMaybe errMsg $ catches V.!? cix
          typeNames <- mapM (\(tix, off) -> liftM (, off) (getTypeName dx tix)) (chHandlers ch)
          return ExceptionRange { erOffset = tryStartAddr tryItem
                                , erCount = tryInsnCount tryItem
                                , erCatch = typeNames
                                , erCatchAll = chAllAddr ch
                                }

-- | Label a stream of raw Dalvik instructions with SSA numbers,
-- adding phi nodes at the beginning of appropriate basic blocks.
--
-- If an argument has no name, it will be assigned a generic one that
-- cannot conflict with the real arguments.
labelInstructions :: (E.MonadThrow m)
                     => DT.DexFile
                     -> [(Maybe BS.ByteString, Word16)]
                     -- ^ A mapping of argument names to the register numbers
                     -> [ExceptionRange]
                     -- ^ Information about exception handlers in the method
                     -> [Instruction]
                     -- ^ The instruction stream for the method
                     -> m Labeling
labelInstructions df argRegs ers is = liftM fst $ evalRWST (label' df) e0 s0
  where
    s0 = emptyLabelState argRegs'
    e0 = emptyEnv argRegs' ers ivec
    ivec = V.fromList is
    argRegs' = zipWith (curry nameAnonArgs) [0..] argRegs
    nameAnonArgs :: (Int, (Maybe BS.ByteString, Word16)) -> (BS.ByteString, Word16)
    nameAnonArgs (ix, (name, reg)) =
      case name of
        Just name' -> (name', reg)
        Nothing -> (generateNameForParameter ix, reg)

-- | An environment to carry state for the labeling algorithm
type SSALabeller m = RWST LabelEnv () LabelState m

-- | Driver for labeling
label' :: (E.MonadThrow m) => DT.DexFile -> SSALabeller m Labeling
label' df = do
  ivec <- asks envInstructionStream
  argRegs <- asks envRegisterAssignment

  -- For each argument to the function, add phantom Phi nodes in the
  -- entry block.  These are not needed in all cases - when they are
  -- not necessary, they will later be removed by
  -- 'tryRemoveTrivialPhi'.  They are required because, without them,
  -- registers containing arguments always appear to have a mapping to
  -- that argument in 'readRegister'.  In cases where there is a loop
  -- backedge to the entry block, this is not actually the case (a phi
  -- is required).
  forM_ (M.toList argRegs) $ \(regNo, _) ->
    makeIncomplete regNo 0

  -- This is the actual labeling step
  mapM_ (labelAndFillInstruction df) $ V.toList $ V.indexed ivec

  -- Now pull out all of the information we need to save to use the
  -- labeling.
  s <- get
  bbs <- asks envBasicBlocks
  argLabels <- asks (M.elems . envRegisterAssignment)
  return Labeling { labelingReadRegs = instructionLabels s
                  , labelingWriteRegs = instructionResultLabels s
                  , labelingPhis = phiOperands s
                  , labelingPhiSources = phiOperandSources s
                  , labelingBlockPhis = foldr (addPhiForBlock s) M.empty $ M.keys (phiOperands s)
                  , labelingBasicBlocks = bbs
                  , labelingInstructions = ivec
                  , labelingParameters = argLabels
                  }
  where
    -- Record the phi nodes introduced in each basic block.  The key
    -- thing to note here is that we do not include phi nodes with no
    -- references.
    addPhiForBlock s p@(PhiLabel bnum _ _) m
      | Just _ <- M.lookup p (phiOperands s)
      , maybe False (not . S.null) $ M.lookup p (valueUsers s)
         = M.insertWith (++) bnum [p] m
      | otherwise = m
    addPhiForBlock _ _ m = m

-- | Label instructions and, if they end a block, mark the block as filled.
--
-- If we do fill a block, we have to see if this filling will let us
-- seal any other blocks.  A block can be sealed if all of its
-- predecessors are filled.
labelAndFillInstruction :: (E.MonadThrow m) => DT.DexFile -> (Int, Instruction) -> SSALabeller m ()
labelAndFillInstruction df i@(ix, _) = do
  bbs <- asks envBasicBlocks
  -- If an instruction doesn't have a block number, it is dead code.
  case instructionBlockNumber bbs ix of
    Nothing -> return ()
    Just bnum -> do
      canSeal <- canSealBlock bnum
      notSealed <- liftM not (blockIsSealed bnum)
      case canSeal && notSealed of
        True -> sealBlock bnum
        False -> return ()

      labelInstruction df i
      case instructionEndsBlock bbs ix of
        False -> return ()
        True -> do
          modify $ \s -> s { filledBlocks = IS.insert bnum (filledBlocks s) }

          -- Check this block and all of its successors.  For each one,
          -- if all predecessors are filled, then seal (unless already sealed)
          succs <- basicBlockSuccessorsM bnum
          succs' <- filterM (liftM not . blockIsSealed) succs
          forM_ succs' $ \ss -> do
            canSealS <- canSealBlock ss
            case canSealS of
              False -> return ()
              True -> sealBlock ss

-- | Algorithm 4 (section 2.3) from the SSA algorithm.  This is called
-- once all of the predecessors of the block are filled.  This
-- computes the phi operands for incomplete phis.
sealBlock :: (E.MonadThrow m) => BlockNumber -> SSALabeller m ()
sealBlock block = do
  iphis <- gets incompletePhis
  let blockPhis = maybe [] M.toList $ M.lookup block iphis
  forM_ blockPhis $ \(reg, l) ->
    addPhiOperands reg block l
  modify makeSealed
  where
    makeSealed s = s { sealedBlocks = IS.insert block (sealedBlocks s) }


-- | Check if the given block is sealed (i.e., all of its predecessors
-- are filled).
blockIsSealed :: (E.MonadThrow m) => BlockNumber -> SSALabeller m Bool
blockIsSealed bname = do
  sealed <- gets sealedBlocks
  return $ IS.member bname sealed

-- | Add an empty phi node as a placeholder.  The operands will be
-- filled in once the block is sealed.
addIncompletePhi :: (E.MonadThrow m) => Word16 -> BlockNumber -> Label -> SSALabeller m ()
addIncompletePhi reg block l = modify addP
  where
    addP s = s { incompletePhis =
                    M.insertWith M.union block (M.singleton reg l) (incompletePhis s)
               }

-- | Check if we can seal a block (we can if all predecessors are filled).
canSealBlock :: (E.MonadThrow m) => BlockNumber -> SSALabeller m Bool
canSealBlock bid = do
  ps <- basicBlockPredecessorsM bid
  go ps
  where
    go [] = return True
    go (p:ps) = do
      f <- isFilled p
      if f then go ps else return False

-- | This is the main part of the algorithm from the paper.  Each
-- instruction is processed separately.  Apply *read* before *write*
-- rules.  This will update the per-instruction register map.  That
-- map will be used for the translation step.
labelInstruction :: (E.MonadThrow m) => DT.DexFile -> (Int, Instruction) -> SSALabeller m ()
labelInstruction df (ix, inst) = do
  bbs <- asks envBasicBlocks
  let instBlock = basicBlockForInstruction bbs ix
  let rr :: (E.MonadThrow m, FromRegister a) => a -> SSALabeller m ()
      rr = recordReadRegister ix inst instBlock
      -- If we are writing a wide register, we have to remove the
      -- mapping for the second register of the pair.
      rw :: (E.MonadThrow m, FromRegister a) => Bool -> a -> SSALabeller m ()
      rw wide = recordWriteRegister wide ix instBlock
      rrs :: (E.MonadThrow m, FromRegister a) => [a] -> SSALabeller m ()
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
    Move mt dst src -> do
      l <- readRegister src instBlock
      recordAssignment ix inst src l
      writeRegisterLabel (mt == DT.MWide) dst instBlock l
      recordAssignment ix inst dst l

    -- This is a pseudo-move that takes an item off of the stack
    -- (following a call or other special instruction) and stuffs it
    -- into a register.  This brings a new value into existence,
    -- unlike normal move.
    Move1 mt dst -> rw (mt == DT.MResultWide) dst
    ReturnVoid -> return ()
    Return _ src -> rr src
    LoadConst dst c -> rw (constWide c) dst
    MonitorEnter src -> rr src
    MonitorExit src -> rr src
    CheckCast src _ -> rr src >> rw False src
    InstanceOf dst src _ -> rr src >> rw False dst
    ArrayLength dst src -> rr src >> rw False dst
    NewInstance dst _ -> rw False dst
    NewArray dst src _ -> rr src >> rw False dst
    FilledNewArray _ srcs -> rrs srcs
    FilledNewArrayRange _ srcs -> rrs srcs
    FillArrayData src _ -> rr src
    Throw src -> rr src
    Goto _ -> return ()
    Goto16 _ -> return ()
    Goto32 _ -> return ()
    PackedSwitch src _ -> rr src
    SparseSwitch src _ -> rr src
    Cmp _ dst src1 src2 -> rrs [src1, src2] >> rw False dst
    If _ src1 src2 _ -> rrs [src1, src2]
    IfZero _ src _ -> rr src
    ArrayOp (Get at) dst src1 src2 -> rrs [src1, src2] >> rw (accessWide at) dst
    ArrayOp (Put _) src3 src1 src2 -> rrs [src1, src2, src3]
    InstanceFieldOp (Get at) dst src _ -> rr src >> rw (accessWide at) dst
    InstanceFieldOp (Put _) src2 src1 _ -> rrs [src1, src2]
    StaticFieldOp (Get at) dst _ -> rw (accessWide at) dst
    StaticFieldOp (Put _) src _ -> rr src
    Invoke ikind _ mid srcs -> do
      srcs' <- lift $ filterWidePairs df mid ikind srcs
      rrs srcs'
    Unop op dst src -> rr src >> rw (unopWide op) dst
    IBinop _ w dst src1 src2 -> rrs [src1, src2] >> rw w dst
    FBinop _ w dst src1 src2 -> rrs [src1, src2] >> rw w dst
    -- These two read and write from the dest register
    IBinopAssign _ w dst src -> rrs [src, dst] >> rw w dst
    FBinopAssign _ w dst src -> rrs [src, dst] >> rw w dst
    -- The literal binop variants do not work on wide types
    BinopLit16 _ dst src _ -> rr src >> rw False dst
    BinopLit8 _ dst src _ -> rr src >> rw False dst
    PackedSwitchData _ _ -> return ()
    SparseSwitchData _ _ -> return ()
    ArrayData _ _ _ -> return ()

-- | True if the unary op works over a wide type
unopWide :: DT.Unop -> Bool
unopWide o =
  case o of
    DT.NegLong -> True
    DT.NotLong -> True
    DT.NegDouble -> True
    _ -> False

-- | True if we are reading or writing a wide value
accessWide :: Maybe DT.AccessType -> Bool
accessWide (Just DT.AWide) = True
accessWide _ = False

-- | True if the constant being loaded is wide (will occupy two registers)
constWide :: DT.ConstArg -> Bool
constWide carg =
  case carg of
    DT.ConstWide16 _ -> True
    DT.ConstWide32 _ -> True
    DT.ConstWide _ -> True
    DT.ConstWideHigh16 _ -> True
    _ -> False

-- | Looks up the mapping for this register at this instruction (from
-- the mutable @currentDefinition@) and record the label mapping for
-- this instruction.
recordReadRegister :: (E.MonadThrow m, FromRegister a) => Int -> Instruction -> BlockNumber -> a -> SSALabeller m ()
recordReadRegister ix inst instBlock srcReg = do
  lbl <- readRegister srcReg instBlock
  recordAssignment ix inst srcReg lbl

-- A possible source of the problem is that we only record the use of
-- the label after we create it.  Possibly a necessary circularity,
-- but it means that a fake phi node is created and eliminated before
-- knowing that it will have a user here.

-- | Create a label for this value, associated with the destination register.
recordWriteRegister :: (E.MonadThrow m, FromRegister a) => Bool -> Int -> BlockNumber -> a -> SSALabeller m ()
recordWriteRegister wide ix instBlock dstReg = do
  lbl <- writeRegister wide dstReg instBlock
  modify (addAssignment lbl)
  where
    addAssignment lbl s =
      let lbls = instructionResultLabels s
      in s { instructionResultLabels = M.insert ix lbl lbls }

-- | At this instruction, associate the new given 'Label' with the
-- named register.  Also add an entry for the reverse mapping (Label
-- is used by this instruction/reg).
recordAssignment :: (E.MonadThrow m, FromRegister a)
                    => Int
                    -> Instruction
                    -> a
                    -> Label
                    -> SSALabeller m ()
recordAssignment ix inst (fromRegister -> reg) lbl =
  modify $ \s ->
  let lbls = instructionLabels s
      users = valueUsers s
  in s { instructionLabels = M.insertWith M.union ix (M.singleton reg lbl) lbls
       , valueUsers = M.insertWith S.union lbl (S.singleton (NormalUse ix inst reg)) users
       }

freshLabel :: (E.MonadThrow m) => SSALabeller m Label
freshLabel = do
  s <- get
  put s { labelCounter = labelCounter s + 1 }
  return $ SimpleLabel (labelCounter s)

freshPhi :: (E.MonadThrow m) => BlockNumber -> SSALabeller m Label
freshPhi bn = do
  preds <- basicBlockPredecessorsM bn
  s <- get
  let lid = labelCounter s
      l = PhiLabel bn preds lid
  put s { labelCounter = lid + 1
        , phiOperands = M.insert l S.empty (phiOperands s)
        }
  return l

-- | Used for instructions that write to a register.  These always
-- define a new value.  From the paper, this is:
--
-- >  writeVariable(variable, block, value):
-- >    currentDef[variable][block] ← value
writeRegister :: (E.MonadThrow m, FromRegister a) => Bool -> a -> BlockNumber -> SSALabeller m Label
writeRegister wide (fromRegister -> reg) block = do
  l <- freshLabel
  writeRegisterLabel wide reg block l
  return l

-- | Write a register with the provided label, instead of allocating a
-- fresh one.
--
-- If the register is wide, we also clear the definition of the second
-- register in the pair (always the next register).  We must do this,
-- otherwise there will be "dangling" definitions where a register
-- pair has been written on one branch, and on a different branch an
-- old definition that was overwritten will appear to be live.  These
-- phantom definitions show up in phi nodes and cannot be resolved
-- (since they never existed in that predecessor branch).
writeRegisterLabel :: (E.MonadThrow m, FromRegister a) => Bool -> a -> BlockNumber -> Label -> SSALabeller m ()
writeRegisterLabel wide (fromRegister -> reg) block l = do
  s <- get
  let defs' = M.insertWith M.union reg (M.singleton block l) (currentDefinition s)
      defs'' = if wide then M.adjust (M.delete block) (reg+1) defs' else defs'
  put s { currentDefinition = defs''
        , registersWithLabel = M.insertWith' S.union l (S.singleton (reg, block)) (registersWithLabel s)
        }


-- | Find the label for a register being read from.  If we have a
-- local definition (due to local variable numbering, i.e., a write in
-- the current block), return that.  Otherwise, check for a global
-- variable numbering.
readRegister :: (E.MonadThrow m, FromRegister a) => a -> BlockNumber -> SSALabeller m Label
readRegister (fromRegister -> reg) block = do
  curDefs <- gets currentDefinition
  case M.lookup reg curDefs of
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
readRegisterRecursive :: (E.MonadThrow m) => Word16 -> BlockNumber -> SSALabeller m Label
readRegisterRecursive reg block = do
  isSealed <- blockIsSealed block
  case isSealed of
    False -> makeIncomplete reg block
    True -> globalNumbering reg block

-- | When a block isn't sealed yet, we add a dummy phi empty phi node.
-- It will be filled in (or eliminated) once the block is sealed.
makeIncomplete :: (E.MonadThrow m) => Word16 -> BlockNumber -> SSALabeller m Label
makeIncomplete r b = do
  p <- freshPhi b
  addIncompletePhi r b p
  writeRegisterLabel False r b p
  return p

-- | global value numbering, with some recursive calls.  The empty phi
-- is inserted to prevent infinite recursion.
globalNumbering :: (E.MonadThrow m) => Word16 -> BlockNumber -> SSALabeller m Label
globalNumbering r b = do
  preds <- basicBlockPredecessorsM b
  l <- case preds of
    [singlePred] -> readRegister r singlePred
    _ -> do
      p <- freshPhi b
      writeRegisterLabel False r b p
      addPhiOperands r b p
  -- FIXME is it okay to say this isn't wide?  I guess nothing has
  -- really been written right here...
  writeRegisterLabel False r b l
  return l

-- | Check if a basic block is filled
isFilled :: (E.MonadThrow m) => BlockNumber -> SSALabeller m Bool
isFilled b = do
  f <- gets filledBlocks
  return $ IS.member b f

-- | Look backwards along control flow edges to find incoming phi
-- values.  These are the operands to the phi node.
addPhiOperands :: (E.MonadThrow m) => Word16 -> BlockNumber -> Label -> SSALabeller m Label
addPhiOperands reg block phi = do
  let PhiLabel _ preds _ = phi
  preds' <- filterM isFilled preds
  forM_ preds' $ \p -> do
    l <- readRegister reg p
    appendPhiOperand phi p l
  -- If this is the entry block, we also need to add in the
  -- contributions from the initial register assignment
  case block == 0 of
    False -> return ()
    True -> do
      argLabels <- asks envRegisterAssignment
      case M.lookup reg argLabels of
        Nothing -> return ()
        Just lbl -> appendPhiOperand phi 0 lbl
  tryRemoveTrivialPhi phi
  remapPhi phi

remapPhi :: (E.MonadThrow m) => Label -> SSALabeller m Label
remapPhi l = do
  ep <- gets eliminatedPhis
  case M.lookup l ep of
    Nothing -> return l
    Just l' -> remapPhi l'

-- | Remove any trivial phi nodes.  The algorithm is a bit aggressive
-- with phi nodes.  This step makes sure we have the minimal number of
-- phi nodes.  A phi node is trivial if it has only one operand
-- (besides itself).  Removing a phi node requires finding all of its
-- uses and replacing them with the trivial (singular) value.
tryRemoveTrivialPhi :: (E.MonadThrow m) => Label -> SSALabeller m ()
tryRemoveTrivialPhi phi = do
  triv <- trivialPhiValue phi
  case triv of
    Nothing -> return ()
    Just tval -> do
      useMap <- gets valueUsers
      -- Note that this list of value users does not include other phi nodes.
      -- This is not a critical problem because we replace phi nodes that are
      -- phi operands separately in 'replacePhiBy'.
      let allUsers = maybe [] S.toList $ M.lookup phi useMap
      replacePhiBy allUsers phi tval

      -- Clear out the old operands to mark this phi node as dead.
      -- This will be important to note during translation (and for
      -- pretty printing).
      modify $ \s -> s { phiOperands = M.insert phi S.empty (phiOperands s)
                       , valueUsers = M.delete phi (valueUsers s)
                       }

      -- Check each user
      forM_ allUsers $ \u ->
        case u of
          PhiUse phi' | phi' /= phi -> do
            -- There is an interesting note here.  This recursive call
            -- to 'tryRemoveTrivialPhi' could remove a phi node that
            -- is already being queried in 'globalValueNumbering'.  If
            -- that is the case, 'globalValueNumbering' won't
            -- necessarily see the change if we just return (and drop
            -- it) here.  Instead, we have a side map of
            -- eliminatedPhis that we can consult to figure out,
            -- globally, which phis have been eliminated.
            --
            -- A cleaner alternative would be to add a layer of
            -- indirection and register uses before we call
            -- 'readRegister'.  Then all users would be registered and
            -- could be updated in this loop.  Unfortunately, that
            -- isn't really possible with the current formulation
            -- since we can't allocate a label before we start
            -- querying.  That would be a good next change.
            tryRemoveTrivialPhi phi'
          _ -> return ()

      modify $ \s -> s { phiOperands = M.delete phi (phiOperands s)
                       , phiOperandSources = M.delete phi (phiOperandSources s)
                       , registersWithLabel = M.delete phi (registersWithLabel s)
                       , eliminatedPhis = M.insert phi tval (eliminatedPhis s)
                       }
      return ()

-- | Replace all of the given uses by the new label provided.
replacePhiBy :: (E.MonadThrow m) => [Use] -> Label -> Label -> SSALabeller m ()
replacePhiBy uses oldPhi trivialValue = do
  regLabs <- gets registersWithLabel
  case M.lookup oldPhi regLabs of
    Nothing -> return ()
    Just rls ->
      F.forM_ rls $ \(r, b) -> do
        modify $ \s ->
          let replaceInBlock = M.insert b trivialValue
          in s { currentDefinition = M.adjust replaceInBlock r (currentDefinition s)
               , registersWithLabel = M.insertWith S.union trivialValue (S.singleton (r, b)) (registersWithLabel s)
               }
  forM_ uses $ \u ->
    case u of
      NormalUse ix inst reg -> recordAssignment ix inst reg trivialValue
      PhiUse phiUser -> do
        let replaceOp = S.insert trivialValue . S.delete oldPhi
        modify $ \s ->
          let opSrcs = fromMaybe M.empty $ M.lookup phiUser (phiOperandSources s)
              bset = fromMaybe S.empty $ M.lookup oldPhi opSrcs
              opSrcs' = M.delete oldPhi $ M.insert trivialValue bset opSrcs
          in s { phiOperands = M.adjust replaceOp phiUser (phiOperands s)
               , valueUsers = M.insertWith' S.union trivialValue (S.singleton u) (valueUsers s)
               , phiOperandSources = M.insert phiUser opSrcs' (phiOperandSources s)
               }
        return ()

-- | A phi node is trivial if it merges two or more distinct values
-- (besides itself).  Unique operands and then remove self references.
-- If there is only one label left, return Just that.
--
-- Note that each phi incoming value label is tagged with the block number
-- that it came from.  This information is important for the SSA translation
-- later on, but it can get in the way here.
trivialPhiValue :: (E.MonadThrow m) => Label -> SSALabeller m (Maybe Label)
trivialPhiValue phi = do
  operandMap <- gets phiOperands
  case M.lookup phi operandMap of
    Just ops ->
      let withoutSelf = S.filter (/=phi) ops
    -- FIXME: If the result of this is empty, the phi node is actually
    -- undefined.  We want a special case for that so we can insert an
    -- undefined instruction in the translation phase.  We can't
    -- really introduce one of these in a syntactically valid Java
    -- program, but the bytecode could probably contain
    -- some... especially malicious bytecode.
    --
    -- That said, the bytecode verifier doesn't allow undefined
    -- references to exist...
      in case S.toList withoutSelf of
        [trivial] -> return (Just trivial)
        _ -> return Nothing
    Nothing -> return Nothing

-- | > appendPhiOperand phi bnum operand
--
-- Adds an @operand@ (that came from basic block @bnum@) to a @phi@
-- node.
appendPhiOperand :: (E.MonadThrow m) => Label -> BlockNumber -> Label -> SSALabeller m ()
appendPhiOperand phi bnum operand = modify appendOperand
  where
    appendOperand s =
      s { phiOperands = M.insertWith S.union phi (S.singleton operand) (phiOperands s)
        , phiOperandSources = M.insertWith' (M.unionWith S.union) phi (M.singleton operand (S.singleton bnum)) (phiOperandSources s)
        , valueUsers = M.insertWith S.union operand (S.singleton (PhiUse phi)) (valueUsers s)
        }

-- | When wide values (longs or doubles) are passed as parameters to
-- methods, *both* registers appear as arguments to the invoke
-- instruction.  This is different than in other instructions, where
-- only the first register is referenced.  We can't do a label/value
-- lookup on the second register since we aren't accounting for them
-- (and don't want them in the argument lists anyway).
--
-- This function filters out the second register in each wide argument
-- pair.
--
-- This prevents us from accidentally processing these extra registers
-- and generating empty/undefined phi labels.  We need this both for
-- the labeling and the SSA translation phases.
filterWidePairs :: (E.MonadThrow m) => DT.DexFile -> DT.MethodId -> DT.InvokeKind -> [DT.Reg16] -> m [DT.Reg16]
filterWidePairs df mId ikind argRegs = do
  m <- getMethod df mId
  p <- getProto df (DT.methProtoId m)
  -- If this is an instance method, be sure to always save the first
  -- argument (since it doesn't appear in the prototype).  To do that,
  -- we have to look up the class of the method and then iterate
  -- through all of the EncodedMethods until we find it.  Lame.
  case ikind of
    DT.Static -> go (DT.protoParams p) argRegs
    _ -> do
      let (this:rest) = argRegs
      rest' <- go (DT.protoParams p) rest
      return (this : rest')
  where
    -- After the types are exhausted, the rest of the arguments must
    -- be varargs, which are explicitly boxed in the IR.
    go [] rest = return rest
    go (tid:tids) (r1:rest) = do
      tyName <- DT.getTypeName df tid
      case BS.unpack tyName of
        "J" -> liftM (r1:) $ dropNextReg tids rest
        "D" -> liftM (r1:) $ dropNextReg tids rest
        _ -> do
          rest' <- go tids rest
          return (r1 : rest')
    go _ [] = E.throwM $ DT.ArgumentTypeMismatch mId argRegs
    dropNextReg _ [] = E.throwM $ DT.ArgumentTypeMismatch mId argRegs
    dropNextReg tids (_:rest) = go tids rest


-- Block predecessors.  These are some monadic wrappers around the
-- real functions.  They just extract the BasicBlocks object from the
-- environment.

-- | A wrapper around 'basicBlockPredecessors' handling the environment lookup
basicBlockPredecessorsM :: (E.MonadThrow m) => BlockNumber -> SSALabeller m [BlockNumber]
basicBlockPredecessorsM bid = do
  bbs <- asks envBasicBlocks
  return $ basicBlockPredecessors bbs bid

-- | A wrapper around 'basicBlockSuccessors' handling the environment lookup
basicBlockSuccessorsM :: (E.MonadThrow m) => BlockNumber -> SSALabeller m [BlockNumber]
basicBlockSuccessorsM bid = do
  bbs <- asks envBasicBlocks
  return $ basicBlockSuccessors bbs bid


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
    PhiLabel phiBlock _ _ -> phiBlock == bid
    _ -> False

-- | A simple pretty printer for computed SSA labels.  This is
-- basically for debugging.
prettyLabeling :: Labeling -> String
prettyLabeling l =
  PP.render $ PP.vcat $ map prettyBlock $ basicBlocksAsList bbs
  where
    bbs = labelingBasicBlocks l
    ivec = labelingInstructions l
    prettyBlock (bid, blockOff, insts) =
      let header = PP.text ";; " PP.<> PP.int bid PP.<> PP.text (show (basicBlockPredecessors bbs bid))
          blockPhis = filter (phiForBlock bid . fst) $ M.toList (labelingPhis l)
          blockPhiDoc = PP.vcat [ PP.text (printf "$%d = phi(%s)" phiL (show vals))
                                | (PhiLabel _ _ phiL, S.toList -> vals) <- blockPhis,
                                  not (null vals)
                                ]
          body = blockPhiDoc $+$ (PP.vcat $ zipWith (curry prettyInst) [blockOff..] $ V.toList insts)
      in header $+$ PP.nest 2 body
    branchTargets i = fromMaybe "??" $ do
      ix <- V.elemIndex i ivec
      srcBlock <- instructionBlockNumber bbs ix
      let targetBlocks = basicBlockSuccessors bbs srcBlock
      return $ L.intercalate ", " (map show targetBlocks)
    prettyInst (ix, i) =
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
          regMap <- M.lookup ix (labelingReadRegs l)
          lab <- M.lookup (fromRegister reg) regMap
          case lab of
            SimpleLabel lnum -> return (show lnum)
            PhiLabel _ _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return (show s)
        wLabelId _reg = fromMaybe "??" $ do
          lab <- M.lookup ix (labelingWriteRegs l)
          case lab of
            SimpleLabel lnum -> return (show lnum)
            PhiLabel _ _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return (show s)

-- Testing property

-- | Returns @True@ if all of the labels assigned to instructions
-- are unique.
generatedLabelsAreUnique :: Labeling -> Bool
generatedLabelsAreUnique =
  snd . foldr checkRepeats (S.empty, True) . M.toList . labelingWriteRegs
  where
    checkRepeats (_, l) (marked, foundRepeat)
      | S.member l marked = (marked, False)
      | otherwise = (S.insert l marked, foundRepeat)

{- Note [Overview]

This implementation of SSA value numbering closely follows the linked
paper.  This note tries to provide a high-level view of the algorithm.
The algorithm runs forward over the instruction stream, visiting each
instruction once.  As it proceeds, it modifies the current register
state, held in the State record

> currentDefinition :: Map Word16 (Map BlockNumber Label)

This is a map of registers to their definitions in each basic block.
Definitions are SSA labels that are referenced at that point.  This
map is destructively modified by each instruction.  We largely use
@readRegister@, @writeRegister@, and @writeRegisterLabel@ to access
this map.  We also maintain two other maps:

> instructionLabels :: Map Int (Map Word16 Label)
> instructionResultLabels :: Map Int Label

These record the labels referenced by each low-level Dalvik
instruction.  @instructionLabels@ are the labels that each instruction
reads.  @instructionResultLabels@ are the labels that instructions
*produce*.  We have to track these separately because some
instructions read from and write to the same register.  Note that once
an entry is made in either of these maps, it never changes (except
when we eliminate trivial phi nodes).  These are also the main
features of the @Labeling@ result.

The basic idea of the algorithm is that, for a given instruction, we
determine what values it references (to populate @instructionLabels@)
by determining what label each register it refers to currently
contains (based on @currentDefinition@).  If there was a definition
earlier in the current basic block, we take that.  This is local value
numbering.  Otherwise, we look at all of the predecessors of the block
and derive the definition from them.  With a single predecessor, this
is trivial.  Otherwise, we start introducing phi nodes.  If the block
is sealed (i.e., all of its predecessors have been fully labeled),
then we recursively look at each predecessor and construct a phi node.
If there are predecessor blocks that haven't been processed yet, we
just leave a dummy phi node to be filled in later.  Every time a basic
block is processed, we try to seal all of its successors (since one of
*their* predecessors has been processed) by filling in those
stubbed-out phi nodes.

Furthermore, every time we finish a phi node, we try to simplify it.
If it is trivial, we remove it and replace it with a simpler
definition.



= Future directions =

 * Copy/constant propagation could allow some more dead code removal

 * With an iterative and interleaved approach, we could actually eliminate
   some superflouous control flow edges related to null pointer exceptions.
   After the first time a pointer is dereferenced, we know that later
   dereferences in the same basic block cannot fail.

 * There are optimizations in the paper for producing the minimal number of
   phi nodes in the presence of irreducable control flow.  Irreducable control
   flow is very rare in practice (especially with compiler-generated code), so
   this isn't very important.

 * There are also a few suggestions in the paper for reducing the number of
   trivial phi nodes that are inserted and later removed.  This isn't a huge
   issue, but it could speed things up a little.  An easier speedup is marked
   with a FIXME above - our approach to replacing phi nodes could be made more
   efficient with an index tracking more precisely where each phi node is used.

-}
