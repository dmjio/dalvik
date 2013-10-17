module Dalvik.SSA.ClassHierarchy (
  ClassHierarchy,
  classHierarchy,
  classHierarchySuperclass,
  classHierarchySuperclassDef,
  classHierarchySubclasses,
  classHierarchyDefinition,
  classHierarchyResolveMethodRef,
  classHierarchyVirtualDispatch
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
classHierarchySuperclass :: ClassHierarchy -> Type -> Maybe Type
classHierarchySuperclass ch t = HM.lookup t (hierarchy ch)

-- | Get any subclasses of the given type
classHierarchySubclasses :: ClassHierarchy -> Type -> [Type]
classHierarchySubclasses ch t = fromMaybe [] $ HM.lookup t (children ch)

-- | Get the definition of the parent of a type, if any
classHierarchySuperclassDef :: ClassHierarchy -> Type -> Maybe Class
classHierarchySuperclassDef ch t = do
  pt <- classHierarchySuperclass ch t
  classHierarchyDefinition ch pt

-- | Return the definition of the given class type if the definition
-- is available in this dex file.
classHierarchyDefinition :: ClassHierarchy -> Type -> Maybe Class
classHierarchyDefinition ch t = HM.lookup t (typeToClassMap ch)

-- | Given a type of a value and a reference to a method to be called
-- on that value, figure out which actual method will be invoked.
--
-- Note, only virtual methods are checked because direct method calls
-- do not need to be resolved.
classHierarchyResolveMethodRef :: ClassHierarchy -> Type -> MethodRef -> Maybe Method
classHierarchyResolveMethodRef ch t0 mref = unsafePerformIO $ go t0
  where
    go t = do
      sc <- MV.readMVar (simpleCache ch)
      case HM.lookup (mref, t) sc of
        Just res -> return res
        Nothing -> do
          let mm = do
                klass <- classHierarchyDefinition ch t
                L.find (matches mref) (classVirtualMethods klass)
          res <- case mm of
            Nothing -> maybe (return Nothing) go (classHierarchySuperclass ch t)
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

classHierarchyVirtualDispatch :: ClassHierarchy
                              -> Instruction -- ^ Invoke instruction
                              -> InvokeVirtualKind -- ^ Type of invocation
                              -> MethodRef -- ^ Method being invoked
                              -> Value -- ^ Receiver object
                              -> Set Method
classHierarchyVirtualDispatch cha i ikind mref receiver
  | ikind == MethodInvokeSuper = maybe S.empty S.singleton $ do
    let bb = instructionBasicBlock i
        lmeth = basicBlockMethod bb
    pt <- classHierarchySuperclass cha (classType (methodClass lmeth))
    classHierarchyResolveMethodRef cha pt mref
  | Just Parameter {} <- fromValue receiver =
    anyHierarchyTarget cha mref (valueType receiver)
  | Just MoveException {} <- fromValue (stripCasts receiver) =
    anyHierarchyTarget cha mref (valueType receiver)
  | otherwise = maybe S.empty S.singleton $ do
    classHierarchyResolveMethodRef cha (valueType receiver) mref

anyHierarchyTarget :: ClassHierarchy -> MethodRef -> Type -> Set Method
anyHierarchyTarget cha mref t0 = unsafePerformIO $ go S.empty t0
  where
    go ms t = do
      cache <- MV.readMVar (dispatchCache cha)
      case HM.lookup (mref, t) cache of
        Just s -> return s
        Nothing -> do
          let ms' = case classHierarchyResolveMethodRef cha t mref of
                Just m -> S.insert m ms
                Nothing -> ms
          res <- foldM go ms' (classHierarchySubclasses cha t)
          MV.modifyMVar_ (dispatchCache cha) $ \c ->
            return $ HM.insert (mref, t) res c
          return res
