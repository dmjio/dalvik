module Dalvik.SSA.ClassHierarchy (
  ClassHierarchy,
  classHierarchy,
  classHierarchySuperclass,
  classHierarchySuperclassDef,
  classHierarchySubclasses,
  classHierarchyDefinition,
  classHierarchyResolveMethodRef
  ) where

import qualified Data.List as L
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( fromMaybe )

import Dalvik.SSA

data ClassHierarchy =
  ClassHierarchy { hierarchy :: HashMap Type Type
                 , children :: HashMap Type [Type]
                 , typeToClassMap :: HashMap Type Class
                 }
  deriving (Eq, Show)

emptyClassHierarchy :: ClassHierarchy
emptyClassHierarchy =
  ClassHierarchy { hierarchy = HM.empty
                 , children = HM.empty
                 , typeToClassMap = HM.empty
                 }

-- | Perform a class hierarchy analysis
classHierarchy :: DexFile -> ClassHierarchy
classHierarchy = foldr addClass emptyClassHierarchy . dexClasses

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
classHierarchyResolveMethodRef ch t mref = do
  klass <- classHierarchyDefinition ch t
  case L.find (matches mref) (classVirtualMethods klass) of
    Nothing -> do
      pt <- classHierarchySuperclass ch t
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
