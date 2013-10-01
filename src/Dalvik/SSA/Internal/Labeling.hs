{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Arrow ( second )
import Control.Failure
import Control.Monad ( filterM, forM_, liftM )
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.ByteString.Char8 as BS
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
           deriving (Eq, Ord, Show)

-- | A labeling assigns an SSA number/Label to a register at *each*
-- 'Instruction'.
data Labeling =
  Labeling { labelingReadRegs :: Map Int (Map Word16 Label)
           , labelingWriteRegs :: Map Int Label
           , labelingPhis :: Map Label (Set Label)
             -- ^ The incoming values for each phi node (phi labels are the map keys)
           , labelingPhiSources :: Map (Label, Label) (Set BlockNumber)
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
    addBlockNumber iv =
      let Just tagged = M.lookup (phi, iv) (labelingPhiSources labeling)
      in zip (S.toList tagged) (repeat iv)

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
             , phiOperands :: Map Label (Set Label)
               -- ^ Operands of each phi node.  They are stored
               -- separately to make them easier to update (since they
               -- do need to be updated in several cases).
             , phiOperandSources :: Map (Label, Label) (Set BlockNumber)
             , incompletePhis :: Map BlockNumber (Map Word16 Label)
               -- ^ These are the phi nodes in each block that cannot
               -- be resolved yet because there are unfilled
               -- predecessor blocks.  Unfilled predecessors could add
               -- definitions that affect the phi node by the time
               -- they are filled.  Once those blocks are filled, the
               -- incomplete phis will be filled in.
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
             , phiOperands = M.empty
             , phiOperandSources = M.empty
             , incompletePhis = M.empty
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
labelMethod :: (Failure DecodeError f) => DT.DexFile -> DT.EncodedMethod -> f Labeling
labelMethod _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
labelMethod dx em@(DT.EncodedMethod _ _ (Just codeItem)) = do
  insts <- DT.decodeInstructions (codeInsns codeItem)
  regMap <- methodRegisterAssignment dx em
  ers <- methodExceptionRanges dx em
  labelInstructions dx regMap ers insts

-- | Parse the try/catch description tables for this 'EncodedMethod'
-- from the DexFile.  The tables are reduced to summaries
-- ('ExceptionRange') that are easier to work with.
methodExceptionRanges :: (Failure DecodeError f) => DT.DexFile -> EncodedMethod -> f [ExceptionRange]
methodExceptionRanges _ (DT.EncodedMethod mId _ Nothing) = failure $ NoCodeForMethod mId
methodExceptionRanges dx (DT.EncodedMethod _ _ (Just codeItem)) =
  mapM toExceptionRange (codeTryItems codeItem)
  where
    catches = V.fromList $ codeHandlers codeItem
    toExceptionRange tryItem = do
      let hOffset = fromIntegral $ tryHandlerOff tryItem
      case V.findIndex ((==hOffset) . chHandlerOff) catches of
        Nothing ->failure $ NoHandlerAtOffset hOffset
        Just cix -> do
          let Just ch = catches V.!? cix
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
labelInstructions :: (Failure DecodeError f)
                     => DT.DexFile
                     -> [(Maybe BS.ByteString, Word16)]
                     -- ^ A mapping of argument names to the register numbers
                     -> [ExceptionRange]
                     -- ^ Information about exception handlers in the method
                     -> [Instruction]
                     -- ^ The instruction stream for the method
                     -> f Labeling
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
type SSALabeller f = RWST LabelEnv () LabelState f

-- | Driver for labeling
label' :: (Failure DecodeError f) => DT.DexFile -> SSALabeller f Labeling
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
    addPhiForBlock s p@(PhiLabel bnum _ _) m
      | Just ivs <- M.lookup p (phiOperands s), not (S.null ivs) = M.insertWith (++) bnum [p] m
      | otherwise = m
    addPhiForBlock _ _ m = m

-- | Label instructions and, if they end a block, mark the block as filled.
--
-- If we do fill a block, we have to see if this filling will let us
-- seal any other blocks.  A block can be sealed if all of its
-- predecessors are filled.
labelAndFillInstruction :: (Failure DecodeError f) => DT.DexFile -> (Int, Instruction) -> SSALabeller f ()
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
          s <- get
          put s { filledBlocks = IS.insert bnum (filledBlocks s) }

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
sealBlock :: (Failure DecodeError f) => BlockNumber -> SSALabeller f ()
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
blockIsSealed :: (Failure DecodeError f) => BlockNumber -> SSALabeller f Bool
blockIsSealed bname = do
  sealed <- gets sealedBlocks
  return $ IS.member bname sealed

-- | Add an empty phi node as a placeholder.  The operands will be
-- filled in once the block is sealed.
addIncompletePhi :: (Failure DecodeError f) => Word16 -> BlockNumber -> Label -> SSALabeller f ()
addIncompletePhi reg block l = modify addP
  where
    addP s = s { incompletePhis =
                    M.insertWith M.union block (M.singleton reg l) (incompletePhis s)
               }

-- | Check if we can seal a block (we can if all predecessors are filled).
canSealBlock :: (Failure DecodeError f) => BlockNumber -> SSALabeller f Bool
canSealBlock bid = do
  ps <- basicBlockPredecessorsM bid
  liftM and $ mapM isFilled ps

-- | This is the main part of the algorithm from the paper.  Each
-- instruction is processed separately.  Apply *read* before *write*
-- rules.  This will update the per-instruction register map.  That
-- map will be used for the translation step.
labelInstruction :: (Failure DecodeError f) => DT.DexFile -> (Int, Instruction) -> SSALabeller f ()
labelInstruction df (ix, inst) = do
  bbs <- asks envBasicBlocks
  let instBlock = basicBlockForInstruction bbs ix
  let rr :: (Failure DecodeError f, FromRegister a) => a -> SSALabeller f ()
      rr = recordReadRegister ix inst instBlock
      -- If we are writing a wide register, we have to remove the
      -- mapping for the second register of the pair.
      rw :: (Failure DecodeError f, FromRegister a) => Bool -> a -> SSALabeller f ()
      rw wide = recordWriteRegister wide ix instBlock
      rrs :: (Failure DecodeError f, FromRegister a) => [a] -> SSALabeller f ()
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
      -- filterWidePairs :: (Failure DecodeError f) => DT.DexFile -> DT.MethodId -> DT.InvokeKind -> [DT.Reg16] -> f [DT.Reg16]
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
recordReadRegister :: (Failure DecodeError f, FromRegister a) => Int -> Instruction -> BlockNumber -> a -> SSALabeller f ()
recordReadRegister ix inst instBlock srcReg = do
  lbl <- readRegister srcReg instBlock
  recordAssignment ix inst srcReg lbl

-- | Create a label for this value, associated with the destination register.
recordWriteRegister :: (Failure DecodeError f, FromRegister a) => Bool -> Int -> BlockNumber -> a -> SSALabeller f ()
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
recordAssignment :: (Failure DecodeError f, FromRegister a)
                    => Int
                    -> Instruction
                    -> a
                    -> Label
                    -> SSALabeller f ()
recordAssignment ix inst (fromRegister -> reg) lbl =
  modify $ \s ->
  let lbls = instructionLabels s
      users = valueUsers s
  in s { instructionLabels = M.insertWith M.union ix (M.singleton reg lbl) lbls
       , valueUsers = M.insertWith S.union lbl (S.singleton (NormalUse ix inst reg)) users
       }

freshLabel :: (Failure DecodeError f) => SSALabeller f Label
freshLabel = do
  s <- get
  put s { labelCounter = labelCounter s + 1 }
  return $ SimpleLabel (labelCounter s)

freshPhi :: (Failure DecodeError f) => BlockNumber -> SSALabeller f Label
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
writeRegister :: (Failure DecodeError f, FromRegister a) => Bool -> a -> BlockNumber -> SSALabeller f Label
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
writeRegisterLabel :: (Failure DecodeError f, FromRegister a) => Bool -> a -> BlockNumber -> Label -> SSALabeller f ()
writeRegisterLabel wide (fromRegister -> reg) block l = do
  s <- get
  let defs' = M.insertWith M.union reg (M.singleton block l) (currentDefinition s)
      defs'' = if wide then M.adjust (M.delete block) (reg+1) defs' else defs'
  put s { currentDefinition = defs'' }


-- | Find the label for a register being read from.  If we have a
-- local definition (due to local variable numbering, i.e., a write in
-- the current block), return that.  Otherwise, check for a global
-- variable numbering.
readRegister :: (Failure DecodeError f, FromRegister a) => a -> BlockNumber -> SSALabeller f Label
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
readRegisterRecursive :: (Failure DecodeError f) => Word16 -> BlockNumber -> SSALabeller f Label
readRegisterRecursive reg block = do
  isSealed <- blockIsSealed block
  case isSealed of
    False -> makeIncomplete reg block
    True -> globalNumbering reg block

-- | When a block isn't sealed yet, we add a dummy phi empty phi node.
-- It will be filled in (or eliminated) once the block is sealed.
makeIncomplete :: (Failure DecodeError f) => Word16 -> BlockNumber -> SSALabeller f Label
makeIncomplete r b = do
  p <- freshPhi b
  addIncompletePhi r b p
  writeRegisterLabel False r b p
  return p

-- | global value numbering, with some recursive calls.  The empty phi
-- is inserted to prevent infinite recursion.
globalNumbering :: (Failure DecodeError f) => Word16 -> BlockNumber -> SSALabeller f Label
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
isFilled :: (Failure DecodeError f) => BlockNumber -> SSALabeller f Bool
isFilled b = do
  f <- gets filledBlocks
  return $ IS.member b f

-- | Look backwards along control flow edges to find incoming phi
-- values.  These are the operands to the phi node.
addPhiOperands :: (Failure DecodeError f) => Word16 -> BlockNumber -> Label -> SSALabeller f Label
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

-- | Remove any trivial phi nodes.  The algorithm is a bit aggressive
-- with phi nodes.  This step makes sure we have the minimal number of
-- phi nodes.  A phi node is trivial if it has only one operand
-- (besides itself).  Removing a phi node requires finding all of its
-- uses and replacing them with the trivial (singular) value.
tryRemoveTrivialPhi :: (Failure DecodeError f) => Label -> SSALabeller f Label
tryRemoveTrivialPhi phi = do
  triv <- trivialPhiValue phi
  case triv of
    Nothing -> return phi
    Just tval -> do
      useMap <- gets valueUsers
      -- Note that this list of value users does not include other phi nodes.
      -- This is not a critical problem because we replace phi nodes that are
      -- phi operands separately in 'replacePhiBy'.
      let allUsers = maybe [] S.toList $ M.lookup phi useMap
          users = filter (isNotThisPhi phi) allUsers
      replacePhiBy users phi tval

      -- Clear out the old operands to mark this phi node as dead.
      -- This will be important to note during translation (and for
      -- pretty printing).
      modify $ \s -> s { phiOperands = M.insert phi S.empty (phiOperands s) }

      -- Check each user
      forM_ users $ \u ->
        case u of
          PhiUse phi' -> do
            _ <- tryRemoveTrivialPhi phi'
            return ()
          _ -> return ()
      return tval
  where
    isNotThisPhi :: Label -> Use -> Bool
    isNotThisPhi _ (NormalUse _ _ _) = True
    isNotThisPhi p (PhiUse l) = p == l

-- | Replace all of the given uses by the new label provided.
--
-- FIXME: This could be made more efficient.  If the new label,
-- @trivialValue@, isn't an argument, then it should be sufficient to
-- use writeRegisterLabel for each use.
replacePhiBy :: (Failure DecodeError f) => [Use] -> Label -> Label -> SSALabeller f ()
replacePhiBy uses oldPhi trivialValue = do
  modify $ \s -> s { phiOperands = M.map (S.map replacePhiUses) (phiOperands s)
                   , phiOperandSources = M.mapKeys (second replacePhiUses) (phiOperandSources s)
                   , currentDefinition = fmap (fmap replacePhiUses) (currentDefinition s)
                   }
  -- Note that we do nothing right now in the phi use case.  This is
  -- because we have handled phis in the above modify call.  That is
  -- heavy handed - if we ever make that more efficient, we might need
  -- to change the PhiUse case here.
  forM_ uses $ \u ->
    case u of
      NormalUse ix inst reg -> recordAssignment ix inst reg trivialValue
      PhiUse _ -> return ()
  where
    replacePhiUses u = if oldPhi == u then trivialValue else u

-- | A phi node is trivial if it merges two or more distinct values
-- (besides itself).  Unique operands and then remove self references.
-- If there is only one label left, return Just that.
--
-- Note that each phi incoming value label is tagged with the block number
-- that it came from.  This information is important for the SSA translation
-- later on, but it can get in the way here.
trivialPhiValue :: (Failure DecodeError f) => Label -> SSALabeller f (Maybe Label)
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
appendPhiOperand :: (Failure DecodeError f) => Label -> BlockNumber -> Label -> SSALabeller f ()
appendPhiOperand phi bnum operand = modify appendOperand
  where
    appendOperand s =
      s { phiOperands = M.insertWith S.union phi (S.singleton operand) (phiOperands s)
        , phiOperandSources = M.insertWith S.union (phi, operand) (S.singleton bnum) (phiOperandSources s)
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
filterWidePairs :: (Failure DecodeError f) => DT.DexFile -> DT.MethodId -> DT.InvokeKind -> [DT.Reg16] -> f [DT.Reg16]
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
    go _ [] = failure $ DT.ArgumentTypeMismatch mId argRegs
    dropNextReg _ [] = failure $ DT.ArgumentTypeMismatch mId argRegs
    dropNextReg tids (_:rest) = go tids rest


-- Block predecessors.  These are some monadic wrappers around the
-- real functions.  They just extract the BasicBlocks object from the
-- environment.

-- | A wrapper around 'basicBlockPredecessors' handling the environment lookup
basicBlockPredecessorsM :: (Failure DecodeError f) => BlockNumber -> SSALabeller f [BlockNumber]
basicBlockPredecessorsM bid = do
  bbs <- asks envBasicBlocks
  return $ basicBlockPredecessors bbs bid

-- | A wrapper around 'basicBlockSuccessors' handling the environment lookup
basicBlockSuccessorsM :: (Failure DecodeError f) => BlockNumber -> SSALabeller f [BlockNumber]
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
  render $ PP.vcat $ map prettyBlock $ basicBlocksAsList bbs
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
