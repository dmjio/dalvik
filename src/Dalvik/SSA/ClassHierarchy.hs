{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Dalvik.SSA.ClassHierarchy (
  ClassHierarchy,
  classHierarchy,
  superclass,
  allSuperclasses,
  definition,
  superclassDef,
  subclasses,
  allSubclasses,
  implementations,
  allImplementations,
  interfaces,
  allInterfaces,
  allSupertypes,
  isAssignableTo,
  resolveMethodRef,
  virtualDispatch,
  anyTarget,
  implementationsOf,
  methodRefMatches,
  findMethods
  ) where

import qualified Data.Foldable as F
import qualified Data.List as L
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as S

import Dalvik.SSA

data ClassHierarchy =
  ClassHierarchy { hierarchy      :: HashMap Type Type
                 , children       :: HashMap Type [Type]
                 , implementors   :: HashMap Type [Type]
                 , typeToClassMap :: HashMap Type Class
                 , assignable     :: HashSet (Type, Type)
                   -- ^ (x, y) ∈ assignable ⇒ an object of type x can
                   -- be assigned to a reference of type y
                 }
  deriving (Eq)

-- | Return all methods in the 'ClassHierarchy' that satisfy a predicate.
findMethods :: (Method -> Bool) ->ClassHierarchy ->  HashSet Method
findMethods predicate cha = HM.foldl' collect HS.empty (typeToClassMap cha)
  where
    collect :: HashSet Method -> Class -> HashSet Method
    collect set cl = L.foldl' (flip HS.insert) set (filteredMethods cl)

    filteredMethods cl = filter predicate (classMethods cl)

emptyClassHierarchy :: ClassHierarchy
emptyClassHierarchy =
  ClassHierarchy { hierarchy = HM.empty
                 , children = HM.empty
                 , implementors = HM.empty
                 , typeToClassMap = HM.empty
                 , assignable = HS.empty
                 }

-- | Perform a class hierarchy analysis
classHierarchy :: DexFile -> ClassHierarchy
classHierarchy df =
  cha0 { assignable = compat }
  where
    cha0 = foldr addClass emptyClassHierarchy (dexClasses df)
    compat = F.foldl' (computeAssignableMatrix cha0) HS.empty (dexTypes df)

-- | True if objects of the first type are assignable to references of
-- the second type
isAssignableTo :: ClassHierarchy -> Type -> Type -> Bool
isAssignableTo cha otype reftype = HS.member (otype, reftype) (assignable cha)

-- | For each type @t@, @t@ can be assigned to a reference of type T
-- or any of its superclasses.  It can also be assigned to any
-- interface implemented by T (or parents of t)
computeAssignableMatrix :: ClassHierarchy -> HashSet (Type, Type) -> Type -> HashSet (Type, Type)
computeAssignableMatrix cha !acc t =
  F.foldl' (\ !a t' -> HS.insert (t, t') a) acc (ctypes ++ ifaces)
  where
    ctypes = t : allSuperclasses cha t
    ifaces = concatMap (interfaces cha) ctypes

addClass :: Class -> ClassHierarchy -> ClassHierarchy
addClass klass ch =
  ch { hierarchy = case classParent klass of
          Nothing -> hierarchy ch
          Just parent -> HM.insert (classType klass) parent (hierarchy ch)
     , children = case classParent klass of
          Nothing -> children ch
          Just parent -> HM.insertWith (++) parent [classType klass] (children ch)
     , implementors =
         L.foldl' (\m i -> HM.insertWith (++) i [classType klass] m)
                  (implementors ch)
                  (classInterfaces klass)
     , typeToClassMap = HM.insert (classType klass) klass (typeToClassMap ch)
     }

-- | Get the parent of a type, if any
superclass :: ClassHierarchy -> Type -> Maybe Type
superclass ch t = HM.lookup t (hierarchy ch)

-- | Get all superclasses of a type
allSuperclasses :: ClassHierarchy -> Type -> [Type]
allSuperclasses ch = tClosure superclass'
  where superclass' = maybe [] return . superclass ch

-- | Get any immediate subclasses of the given type
subclasses :: ClassHierarchy -> Type -> [Type]
subclasses ch t = fromMaybe [] $ HM.lookup t (children ch)

-- | Get all subclasses transitively of the given type
allSubclasses :: ClassHierarchy -> Type -> [Type]
allSubclasses ch = tClosure (subclasses ch)

-- | Get the types that implement the given interface directly
implementations :: ClassHierarchy -> Type -> [Type]
implementations ch t = fromMaybe [] $ HM.lookup t (implementors ch)

-- | Get the types implemented by the given interface or its subinterfaces
allImplementations :: ClassHierarchy -> Type -> [Type]
allImplementations ch = tClosure (implementations ch)

-- | Get all interfaces directly implemented by the given type
interfaces :: ClassHierarchy -> Type -> [Type]
interfaces ch t =
  fromMaybe [] $ classInterfaces `fmap` (HM.lookup t (typeToClassMap ch))

-- | Get all interfaces transitively implemented by the given type
allInterfaces :: ClassHierarchy -> Type -> [Type]
allInterfaces ch = tClosure (interfaces ch)

-- | Get all supertypes of the given type, including concrete classes,
-- abstract classes, and interfaces
allSupertypes :: ClassHierarchy -> Type -> [Type]
allSupertypes ch t = allSuperclasses ch t ++ allInterfaces ch t

tClosure :: (a -> [a]) -> a -> [a]
tClosure f x = concatMap (rtClosure f) (f x)

rtClosure :: (a -> [a]) -> a -> [a]
rtClosure f x = x : concatMap (rtClosure f) (f x)

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
resolveMethodRef ch t0 mref = go t0
  where
    go t =
      case definition ch t of
        Nothing -> maybe Nothing go (superclass ch t)
        Just klass ->
          case F.foldl' (mostSpecificMatch mref) Nothing (classVirtualMethods klass) of
            Nothing -> maybe Nothing go (superclass ch t)
            res@(Just _) -> res

mostSpecificMatch :: MethodRef -> Maybe Method -> Method -> Maybe Method
mostSpecificMatch mref acc m
  | methodRefName mref == methodName m &&
    methodRefParameterTypes mref == map parameterType ps =
      case acc of
        Nothing -> Just m
        Just m' -> Just $ takeMoreSpecific m' m
  | otherwise = acc
  where
    _:ps = methodParameters m
    object = ReferenceType $ qualifiedClassName ["java", "lang"] "Object"
    takeMoreSpecific m1 m2 =
      case methodReturnType m1 == object of
        True -> m2
        False -> m1

methodRefMatches :: MethodRef -> Method -> Bool
methodRefMatches mref m = methodRefName mref == methodName m &&
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
                   -> Maybe Method
virtualDispatch cha i ikind mref receiver
  | ikind == MethodInvokeSuper = do
    let bb = instructionBasicBlock i
        lmeth = basicBlockMethod bb
    pt <- superclass cha (classType (methodClass lmeth))
    resolveMethodRef cha pt mref
  | otherwise = resolveMethodRef cha (valueType receiver) mref

-- | Find all possible targets for a call to the given 'MethodRef'
-- from a value of the given 'Type'.
anyTarget :: ClassHierarchy -> Instruction -> InvokeVirtualKind -> MethodRef -> Type -> Set Method
anyTarget cha i k mref t0 =
  case k of
    MethodInvokeInterface -> implementationsOfInterfaceMethod cha mref
    MethodInvokeSuper ->
      maybe S.empty S.singleton $ resolveMethodRef cha (methodRefClass mref) mref
    _ -> go S.empty rootType
  where
    rootType = if k /= MethodInvokeSuper then t0 else fromMaybe t0 (superclass cha t0)
    go ms t =
      let ms' = case resolveMethodRef cha t mref of
            Just m -> S.insert m ms
            Nothing -> ms
      in F.foldl' go ms' (subclasses cha t)

-- | Given a class (or interface) name and a method name, find all of
-- the 'Method's implementing that interface method.
--
-- Algorithm:
--
-- 1) Find all classes implementing the named interface (if any)
--
-- 2) Look up the name as if it were a class.
--
-- 3) These are the roots of the search; for each of these classes,
--    find all of the matching methods in the class (one or zero) and
--    then recursively look at all subclasses.
--
-- Note that this is a linear pass over all of the classes in the
-- hierarchy, and won't be cheap.
--
-- The 'MethodRef' is only used to get the method name and signature.
-- You could safely create one manually with a dummy 'methodRefId' and
-- 'methodRefClass' (since neither is consulted).
--
-- Only virtual methods are searched because there is no dispatch for
-- direct methods.
implementationsOf :: ClassHierarchy -> ClassName -> MethodRef -> [Method]
implementationsOf ch klassName mref =
  foldr go [] rootClasses
  where
    t0 = ReferenceType klassName
    typesImplementing = fromMaybe [] $ HM.lookup (ReferenceType klassName) (implementors ch)
    classesImplementing = mapMaybe (definition ch) typesImplementing
    mnamedClass = HM.lookup t0 (typeToClassMap ch)
    rootClasses = maybe classesImplementing (:classesImplementing) mnamedClass
    go klass acc =
      let ms = filter (methodRefMatches mref) $ classVirtualMethods klass
          subs = mapMaybe (definition ch) $ subclasses ch (classType klass)
      in foldr go (ms ++ acc) subs


implementationsOfInterfaceMethod :: ClassHierarchy -> MethodRef -> Set Method
implementationsOfInterfaceMethod cha mref =
  foldr go S.empty (mapMaybe (definition cha) roots)
  where
    roots = fromMaybe [] $ HM.lookup (methodRefClass mref) (implementors cha)
    go klass acc =
      let ms = filter (methodRefMatches mref) $ classVirtualMethods klass
          subs = mapMaybe (definition cha) $ subclasses cha (classType klass)
      in foldr go (acc `S.union` S.fromList ms) subs
