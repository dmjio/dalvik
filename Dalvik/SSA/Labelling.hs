{-# LANGUAGE ViewPatterns #-}
-- | This module implements SSA value numbering over the low-level Dalvik IR
--
-- The algorithm used is from Braun et al
-- (<http://www.cdl.uni-saarland.de/papers/bbhlmz13cc.pdf>).  The
-- labelling maps each operand of a low-level Dalvik instruction to
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
module Dalvik.SSA.Labelling (
  -- * Data Types
  Label(..),
  Labelling(..),
  labelInstructions,
  -- * Testing
  prettyLabelling,
  generatedLabelsAreUnique
  ) where

import Control.Monad ( filterM, forM_, liftM, void )
import Control.Monad.Trans.RWS.Strict
import Data.Int ( Int64 )
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

import Dalvik.Instruction
import Dalvik.SSA.BasicBlocks

-- | Types of label.  Arguments and Phi nodes have special labels
-- carrying extra information.
data Label = SimpleLabel Int64
           | PhiLabel BlockNumber [BlockNumber] Int64
           | ArgumentLabel String Int64
           deriving (Eq, Ord, Show)

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

-- | The mutable state we are modifying while labelling.  This mainly
-- models the register state and phi operands.
data LabelState =
  LabelState { currentDefinition :: Map Word16 (Map BlockNumber Label)
               -- ^ We track the current definition of each register at
               -- each block.
             , instructionLabels :: Map Instruction (Map Word16 Label)
               -- ^ This is separate from @currentDefinition@ because
               -- that field is mutated.
             , instructionResultLabels :: Map Instruction Label
               -- ^ Store the label of instructions that produce
               -- a result separately from the normal map.  This is only
               -- really required because of the compound instructions
               -- 'IBinopAssign' and 'FBinopAssign'
             , phiOperands :: Map Label (Set Label)
               -- ^ Operands of each phi node.  They are stored
               -- separately to make them easier to update (since they
               -- do need to be updated in several cases).
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
             , labelCounter :: Int64
               -- ^ A source of label unique IDs
             }

data Use = NormalUse Instruction Word16
         | PhiUse Label
         deriving (Eq, Ord, Show)

data LabelEnv =
  LabelEnv { envInstructionStream :: Vector Instruction
           , envBasicBlocks :: BasicBlocks
             -- ^ Information about basic blocks in the instruction
             -- stream
           , envRegisterAssignment :: Map Word16 Label
             -- ^ The map of argument registers to their labels
           , envInstructionIndices :: Map Instruction Int
             -- ^ A reverse map from instructions to their integer
             -- indices into the instruction stream
           }



-- | Create a new empty SSA labelling environment.  This includes
-- computing CFG information.
emptyEnv :: [(String, Word16)] -> Vector Instruction -> LabelEnv
emptyEnv argRegs ivec =
  LabelEnv { envInstructionStream = ivec
           , envBasicBlocks = findBasicBlocks ivec
           , envRegisterAssignment =
             fst $ L.foldl' allocateArgLabel (M.empty, 0) argRegs
           , envInstructionIndices = V.ifoldl' addIndex M.empty ivec
           }
  where
    addIndex m ix inst = M.insert inst ix m
    allocateArgLabel (m, lno) (name, reg) =
      (M.insert reg (ArgumentLabel name lno) m, lno + 1)

-- | Create a new empty state.  This state is initialized to account
-- for method arguments, which occupy the last @length argRegs@
-- registers.  Wide arguments take two registers, but we only need to
-- worry about the first register in the pair (since the second is
-- never explicitly accessed).
emptyLabelState :: [(String, Word16)] -> LabelState
emptyLabelState argRegs =
  LabelState { currentDefinition = fst $ L.foldl' addArgDef (M.empty, 0) argRegs
             , instructionLabels = M.empty
             , instructionResultLabels = M.empty
             , phiOperands = M.empty
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

-- | Label a stream of raw Dalvik instructions with SSA numbers,
-- adding phi nodes at the beginning of appropriate basic blocks.
labelInstructions :: [(String, Word16)]
                     -- ^ A mapping of argument names to the register numbers
                     -> [Instruction]
                     -- ^ The instruction stream for the method
                     -> Labelling
labelInstructions argRegs is = fst $ evalRWS label' e0 s0
  where
    s0 = emptyLabelState argRegs
    e0 = emptyEnv argRegs ivec
    ivec = V.fromList is

-- | An environment to carry state for the labelling algorithm
type SSALabeller = RWS LabelEnv () LabelState

-- | Driver for labelling
label' :: SSALabeller Labelling
label' = do
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
  forM_ (M.toList argRegs) $ \(regNo, _) -> do
    makeIncomplete regNo 0

  -- This is the actual labelling step
  mapM_ labelAndFillInstruction $ V.toList $ V.indexed ivec

  -- Now pull out all of the information we need to save to use the
  -- labelling.
  s <- get
  bbs <- asks envBasicBlocks
  return $ Labelling { labellingReadRegs = instructionLabels s
                     , labellingWriteRegs = instructionResultLabels s
                     , labellingPhis = phiOperands s
                     , labellingBasicBlocks = bbs
                     , labellingInstructions = ivec
                     }

-- | Label instructions and, if they end a block, mark the block as filled.
--
-- If we do fill a block, we have to see if this filling will let us
-- seal any other blocks.  A block can be sealed if all of its
-- predecessors are filled.
labelAndFillInstruction :: (Int, Instruction) -> SSALabeller ()
labelAndFillInstruction i@(ix, _) = do
  bbs <- asks envBasicBlocks
  let Just bnum = instructionBlockNumber bbs ix
  canSeal <- canSealBlock bnum
  notSealed <- liftM not (blockIsSealed bnum)
  case canSeal && notSealed of
    True -> sealBlock bnum
    False -> return ()

  labelInstruction i
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
sealBlock :: BlockNumber -> SSALabeller ()
sealBlock block = do
  iphis <- gets incompletePhis
  let blockPhis = maybe [] M.toList $ M.lookup block iphis
  forM_ blockPhis $ \(reg, l) -> do
    addPhiOperands reg block l
  modify makeSealed
  where
    makeSealed s = s { sealedBlocks = IS.insert block (sealedBlocks s) }


-- | Check if the given block is sealed (i.e., all of its predecessors
-- are filled).
blockIsSealed :: BlockNumber -> SSALabeller Bool
blockIsSealed bname = do
  sealed <- gets sealedBlocks
  return $ IS.member bname sealed

-- | Add an empty phi node as a placeholder.  The operands will be
-- filled in once the block is sealed.
addIncompletePhi :: Word16 -> BlockNumber -> Label -> SSALabeller ()
addIncompletePhi reg block l = modify addP
  where
    addP s = s { incompletePhis =
                    M.insertWith M.union block (M.singleton reg l) (incompletePhis s)
               }

-- | Check if we can seal a block (we can if all predecessors are filled).
canSealBlock :: BlockNumber -> SSALabeller Bool
canSealBlock bid = do
  ps <- basicBlockPredecessorsM bid
  liftM and $ mapM isFilled ps

-- | This is the main part of the algorithm from the paper.  Each
-- instruction is processed separately.  Apply *read* before *write*
-- rules.  This will update the per-instruction register map.  That
-- map will be used for the translation step.
labelInstruction :: (Int, Instruction) -> SSALabeller ()
labelInstruction (ix, inst) = do
  bbs <- asks envBasicBlocks
  let instBlock = basicBlockForInstruction bbs ix
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

-- | Looks up the mapping for this register at this instruction (from
-- the mutable @currentDefinition@) and record the label mapping for
-- this instruction.
recordReadRegister :: (FromRegister a) => Instruction -> BlockNumber -> a -> SSALabeller ()
recordReadRegister inst instBlock srcReg = do
  lbl <- readRegister srcReg instBlock
  recordAssignment inst srcReg lbl

-- | Create a label for this value, associated with the destination register.
recordWriteRegister :: (FromRegister a) => Instruction -> BlockNumber -> a -> SSALabeller ()
recordWriteRegister inst instBlock dstReg = do
  lbl <- writeRegister dstReg instBlock
  modify (addAssignment lbl)
  where
    addAssignment lbl s =
      let lbls = instructionResultLabels s
      in s { instructionResultLabels = M.insert inst lbl lbls }

-- | At this instruction, associate the new given 'Label' with the
-- named register.  Also add an entry for the reverse mapping (Label
-- is used by this instruction/reg).
recordAssignment :: (FromRegister a)
                    => Instruction
                    -> a
                    -> Label
                    -> SSALabeller ()
recordAssignment inst (fromRegister -> reg) lbl =
  modify $ \s ->
  let lbls = instructionLabels s
      users = valueUsers s
  in s { instructionLabels = M.insertWith M.union inst (M.singleton reg lbl) lbls
       , valueUsers = M.insertWith S.union lbl (S.singleton (NormalUse inst reg)) users
       }

freshLabel :: SSALabeller Label
freshLabel = do
  s <- get
  put s { labelCounter = labelCounter s + 1 }
  return $ SimpleLabel (labelCounter s)

freshPhi :: BlockNumber -> SSALabeller Label
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
writeRegister :: (FromRegister a) => a -> BlockNumber -> SSALabeller Label
writeRegister (fromRegister -> reg) block = do
  l <- freshLabel
  writeRegisterLabel reg block l
  return l

-- | Write a register with the provided label, instead of allocating a
-- fresh one.
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
readRegisterRecursive :: Word16 -> BlockNumber -> SSALabeller Label
readRegisterRecursive reg block = do
  isSealed <- blockIsSealed block
  case isSealed of
    False -> makeIncomplete reg block
    True -> globalNumbering reg block

makeIncomplete :: Word16 -> BlockNumber -> SSALabeller Label
makeIncomplete r b = do
  p <- freshPhi b
  addIncompletePhi r b p
  writeRegisterLabel r b p
  return p

-- | global value numbering, with some recursive calls.  The empty phi
-- is inserted to prevent infinite recursion.
globalNumbering :: Word16 -> BlockNumber -> SSALabeller Label
globalNumbering r b = do
  preds <- basicBlockPredecessorsM b
  l <- case preds of
    [singlePred] -> readRegister r singlePred
    _ -> do
      p <- freshPhi b
      writeRegisterLabel r b p
      addPhiOperands r b p
  writeRegisterLabel r b l
  return l

-- | Check if a basic block is filled
isFilled :: BlockNumber -> SSALabeller Bool
isFilled b = do
  f <- gets filledBlocks
  return $ IS.member b f

-- | Look backwards along control flow edges to find incoming phi
-- values.  These are the operands to the phi node.
addPhiOperands :: Word16 -> BlockNumber -> Label -> SSALabeller Label
addPhiOperands reg block phi = do
  let PhiLabel _ preds _ = phi
  preds' <- filterM isFilled preds
  forM_ preds' $ \p -> do
    l <- readRegister reg p
    appendPhiOperand phi l
  -- If this is the entry block, we also need to add in the
  -- contributions from the initial register assignment
  case block == 0 of
    False -> return ()
    True -> do
      argLabels <- asks envRegisterAssignment
      case M.lookup reg argLabels of
        Nothing -> return ()
        Just lbl -> appendPhiOperand phi lbl
  tryRemoveTrivialPhi phi

-- | Remove any trivial phi nodes.  The algorithm is a bit aggressive
-- with phi nodes.  This step makes sure we have the minimal number of
-- phi nodes.  A phi node is trivial if it has only one operand
-- (besides itself).  Removing a phi node requires finding all of its
-- uses and replacing them with the trivial (singular) value.
tryRemoveTrivialPhi :: Label -> SSALabeller Label
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
          PhiUse phi' -> void $ tryRemoveTrivialPhi phi'
          _ -> return ()
      return tval
  where
    isNotThisPhi :: Label -> Use -> Bool
    isNotThisPhi _ (NormalUse _ _) = False
    isNotThisPhi p (PhiUse l) = p == l

-- | Replace all of the given uses by the new label provided.
--
-- FIXME: This could be made more efficient.  If the new label,
-- @trivialValue@, isn't an argument, then it should be sufficient to
-- use writeRegisterLabel for each use.
replacePhiBy :: [Use] -> Label -> Label -> SSALabeller ()
replacePhiBy uses oldPhi trivialValue = do
  modify $ \s -> s { phiOperands = M.map (S.map replacePhiUses) (phiOperands s)
                   , currentDefinition = fmap (fmap replacePhiUses) (currentDefinition s)
                   }
  -- Note that we do nothing right now in the phi use case.  This is
  -- because we have handled phis in the above modify call.  That is
  -- heavy handed - if we ever make that more efficient, we might need
  -- to change the PhiUse case here.
  forM_ uses $ \u ->
    case u of
      NormalUse inst reg -> recordAssignment inst reg trivialValue
      PhiUse _ -> return ()
  where
    replacePhiUses u = if oldPhi == u  then trivialValue else u

-- | A phi node is trivial if it merges two or more distinct values
-- (besides itself).  Unique operands and then remove self references.
-- If there is only one label left, return Just that.
trivialPhiValue :: Label -> SSALabeller (Maybe Label)
trivialPhiValue phi = do
  operandMap <- gets phiOperands
  case M.lookup phi operandMap of
    Just ops ->
      let withoutSelf = S.filter (/=phi) ops
      in case S.toList withoutSelf of
        [trivial] -> return (Just trivial)
        _ -> return Nothing
    Nothing -> return Nothing

appendPhiOperand :: Label -> Label -> SSALabeller ()
appendPhiOperand phi operand = modify appendOperand
  where
    appendOperand s =
      s { phiOperands = M.insertWith S.union phi (S.singleton operand) (phiOperands s)
        , valueUsers = M.insertWith S.union phi (S.singleton (PhiUse operand)) (valueUsers s)
        }

-- Block predecessors.  These are some monadic wrappers around the
-- real functions.  They just extract the BasicBlocks object from the
-- environment.

basicBlockPredecessorsM :: BlockNumber -> SSALabeller [BlockNumber]
basicBlockPredecessorsM bid = do
  bbs <- asks envBasicBlocks
  return $ basicBlockPredecessors bbs bid

basicBlockSuccessorsM :: BlockNumber -> SSALabeller [BlockNumber]
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
prettyLabelling :: Labelling -> String
prettyLabelling l =
  render $ PP.vcat $ map prettyBlock $ basicBlocksAsList bbs
  where
    bbs = labellingBasicBlocks l
    ivec = labellingInstructions l
    prettyBlock (bid, insts) =
      let header = PP.text ";; " <> PP.int bid <> PP.text (show (basicBlockPredecessors bbs bid))
          blockPhis = filter (phiForBlock bid . fst) $ M.toList (labellingPhis l)
          blockPhiDoc = PP.vcat [ PP.text (printf "$%d = phi(%s)" phiL (show vals))
                                | (PhiLabel _ _ phiL, (S.toList -> vals)) <- blockPhis,
                                  not (null vals)
                                ]
          body = blockPhiDoc $+$ (PP.vcat $ map prettyInst $ V.toList insts)
      in header $+$ PP.nest 2 body
    branchTargets i = fromMaybe "??" $ do
      ix <- V.elemIndex i ivec
      srcBlock <- instructionBlockNumber bbs ix
      let targetBlocks = basicBlockSuccessors bbs srcBlock
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
            PhiLabel _ _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return s
        wLabelId _reg = fromMaybe "??" $ do
          lab <- M.lookup i (labellingWriteRegs l)
          case lab of
            SimpleLabel lnum -> return (show lnum)
            PhiLabel _ _ lnum -> return (show lnum)
            ArgumentLabel s _ -> return s

-- Testing property

-- | Returns @True@ if all of the labels assigned to instructions
-- are unique.
generatedLabelsAreUnique :: Labelling -> Bool
generatedLabelsAreUnique =
  snd . foldr checkRepeats (S.empty, True) . M.toList . labellingWriteRegs
  where
    checkRepeats (_, l) (marked, foundRepeat)
      | S.member l marked = (marked, False)
      | otherwise = (S.insert l marked, foundRepeat)
