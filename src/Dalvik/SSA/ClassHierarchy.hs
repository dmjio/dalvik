module Dalvik.SSA.ClassHierarchy (
  ClassHierarchy,
  classHierarchy,
  classHierarchyParent,
  classHierarchyParentDef,
  classHierarchyDefinition,
  classHierarchyResolveMethodRef
  ) where

import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M

import Dalvik.SSA

data ClassHierarchy =
  ClassHierarchy { hierarchy :: Map Type Type
                 , typeToClassMap :: Map Type Class
                 }
  deriving (Eq, Ord, Show)

emptyClassHierarchy :: ClassHierarchy
emptyClassHierarchy =
  ClassHierarchy { hierarchy = M.empty
                 , typeToClassMap = M.empty
                 }

-- | Perform a class hierarchy analysis
classHierarchy :: DexFile -> ClassHierarchy
classHierarchy = foldr addClass emptyClassHierarchy . dexClasses

addClass :: Class -> ClassHierarchy -> ClassHierarchy
addClass klass ch =
  ch { hierarchy = case classParent klass of
          Nothing -> hierarchy ch
          Just parent -> M.insert (classType klass) parent (hierarchy ch)
     , typeToClassMap = M.insert (classType klass) klass (typeToClassMap ch)
     }

-- | Get the parent of a type, if any
classHierarchyParent :: ClassHierarchy -> Type -> Maybe Type
classHierarchyParent ch t = M.lookup t (hierarchy ch)

-- | Get the definition of the parent of a type, if any
classHierarchyParentDef :: ClassHierarchy -> Type -> Maybe Class
classHierarchyParentDef ch t = do
  pt <- classHierarchyParent ch t
  classHierarchyDefinition ch pt

-- | Return the definition of the given class type if the definition
-- is available in this dex file.
classHierarchyDefinition :: ClassHierarchy -> Type -> Maybe Class
classHierarchyDefinition ch t = M.lookup t (typeToClassMap ch)

-- | Given a type of a value and a reference to a method to be called
-- on that value, figure out which actual method will be invoked.
--
-- Note, only virtual methods are checked because direct method calls
-- do not need to be resolved.
classHierarchyResolveMethodRef :: ClassHierarchy -> Type -> MethodRef -> Maybe Method
classHierarchyResolveMethodRef ch t mref = do
  klass <- classHierarchyDefinition ch t
  case L.find (matches mref) (classVirtualMethods klass) of
    Nothing -> do
      pt <- classHierarchyParent ch t
      classHierarchyResolveMethodRef ch pt mref
    Just m -> return m

matches :: MethodRef -> Method -> Bool
matches mref m = methodRefName mref == methodName m &&
                   methodRefReturnType mref == methodReturnType m &&
                   methodRefParameterTypes mref == map parameterType ps
  where
    -- Irrefutable pattern match since we are only checking virtual
    -- methods and they all have a @this@ parameter that we need to
    -- ignore (since MethodRefs do not include @this@)
    _:ps = methodParameters m
