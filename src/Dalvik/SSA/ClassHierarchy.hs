module Dalvik.SSA.ClassHierarchy (
  ClassHierarchy,
  classHierarchy,
  superclass,
  superclassDef,
  subclasses,
  resolveMethodRef,
  virtualDispatch,
  anyTarget
  ) where

import Control.Concurrent.MVar as MV
import Control.Monad ( foldM )
import qualified Data.List as L
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( fromMaybe )
import Data.Set ( Set )
import qualified Data.Set as S
import System.IO.Unsafe ( unsafePerformIO )

import Dalvik.SSA

data ClassHierarchy =
  ClassHierarchy { hierarchy :: HashMap Type Type
                 , children :: HashMap Type [Type]
                 , typeToClassMap :: HashMap Type Class
                 , dispatchCache :: MVar (HashMap (MethodRef, Type) (Set Method))
                 , simpleCache :: MVar (HashMap (MethodRef, Type) (Maybe Method))
                 }
  deriving (Eq)

emptyClassHierarchy :: MVar (HashMap (MethodRef, Type) (Set Method))
                       -> MVar (HashMap (MethodRef, Type) (Maybe Method))
                       -> ClassHierarchy
emptyClassHierarchy mv1 mv2 =
  ClassHierarchy { hierarchy = HM.empty
                 , children = HM.empty
                 , typeToClassMap = HM.empty
                 , dispatchCache = mv1
                 , simpleCache = mv2
                 }

-- | Perform a class hierarchy analysis
classHierarchy :: DexFile -> ClassHierarchy
classHierarchy df = unsafePerformIO $ do
  mv1 <- MV.newMVar HM.empty
  mv2 <- MV.newMVar HM.empty
  return $ foldr addClass (emptyClassHierarchy mv1 mv2) $ dexClasses df

addClass :: Class -> ClassHierarchy -> ClassHierarchy
addClass klass ch =
  ch { hierarchy = case classParent klass of
          Nothing -> hierarchy ch
          Just parent -> HM.insert (classType klass) parent (hierarchy ch)
     , children = case classParent klass of
          Nothing -> children ch
          Just parent -> HM.insertWith (++) parent [classType klass] (children ch)
     , typeToClassMap = HM.insert (classType klass) klass (typeToClassMap ch)
     }

-- | Get the parent of a type, if any
superclass :: ClassHierarchy -> Type -> Maybe Type
superclass ch t = HM.lookup t (hierarchy ch)

-- | Get any subclasses of the given type
subclasses :: ClassHierarchy -> Type -> [Type]
subclasses ch t = fromMaybe [] $ HM.lookup t (children ch)

-- | Get the definition of the parent of a type, if any
superclassDef :: ClassHierarchy -> Type -> Maybe Class
superclassDef ch t = do
  pt <- superclass ch t
  definition ch pt

-- | Return the definition of the given class type if the definition
-- is available in this dex file.
definition :: ClassHierarchy -> Type -> Maybe Class
definition ch t = HM.lookup t (typeToClassMap ch)

-- | Given a type of a value and a reference to a method to be called
-- on that value, figure out which actual method will be invoked.
--
-- Note, only virtual methods are checked because direct method calls
-- do not need to be resolved.
resolveMethodRef :: ClassHierarchy -> Type -> MethodRef -> Maybe Method
resolveMethodRef ch t0 mref = unsafePerformIO $ go t0
  where
    go t = do
      sc <- MV.readMVar (simpleCache ch)
      case HM.lookup (mref, t) sc of
        Just res -> return res
        Nothing -> do
          let mm = do
                klass <- definition ch t
                L.find (matches mref) (classVirtualMethods klass)
          res <- case mm of
            Nothing -> maybe (return Nothing) go (superclass ch t)
            Just _ -> return mm
          MV.modifyMVar_ (simpleCache ch) $ \c ->
            return $ HM.insert (mref, t) res c
          return res

matches :: MethodRef -> Method -> Bool
matches mref m = methodRefName mref == methodName m &&
                   methodRefReturnType mref == methodReturnType m &&
                   methodRefParameterTypes mref == map parameterType ps
  where
    -- Irrefutable pattern match since we are only checking virtual
    -- methods and they all have a @this@ parameter that we need to
    -- ignore (since MethodRefs do not include @this@)
    _:ps = methodParameters m

virtualDispatch :: ClassHierarchy
                   -> Instruction -- ^ Invoke instruction
                   -> InvokeVirtualKind -- ^ Type of invocation
                   -> MethodRef -- ^ Method being invoked
                   -> Value -- ^ Receiver object
                   -> Set Method
virtualDispatch cha i ikind mref receiver
  | ikind == MethodInvokeSuper = maybe S.empty S.singleton $ do
    let bb = instructionBasicBlock i
        lmeth = basicBlockMethod bb
    pt <- superclass cha (classType (methodClass lmeth))
    resolveMethodRef cha pt mref
  | Just Parameter {} <- fromValue receiver =
    anyTarget cha ikind mref (valueType receiver)
  | Just MoveException {} <- fromValue (stripCasts receiver) =
    anyTarget cha ikind mref (valueType receiver)
  | otherwise = maybe S.empty S.singleton $ do
    resolveMethodRef cha (valueType receiver) mref

-- | Find all possible targets for a call to the given 'MethodRef'
-- from a value of the given 'Type'.
anyTarget :: ClassHierarchy -> InvokeVirtualKind -> MethodRef -> Type -> Set Method
anyTarget cha k mref t0 = unsafePerformIO $ go S.empty rootType
  where
    rootType = if k /= MethodInvokeSuper then t0 else fromMaybe t0 (superclass cha t0)
    go ms t = do
      cache <- MV.readMVar (dispatchCache cha)
      case Nothing of -- HM.lookup (mref, t) cache of
        Just s -> return s
        Nothing -> do
          let ms' = case resolveMethodRef cha t mref of
                Just m -> S.insert m ms
                Nothing -> ms
          res <- foldM go ms' (subclasses cha t)
          -- MV.modifyMVar_ (dispatchCache cha) $ \c ->
          --   return $ HM.insert (mref, t) res c
          return res
