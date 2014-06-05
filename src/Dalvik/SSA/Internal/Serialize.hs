{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Dalvik.SSA.Internal.Serialize (
  deserializeDex,
  serializeDex
  ) where

import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import qualified Data.Serialize as S

import Dalvik.SSA.Types

deserializeDex :: BS.ByteString -> Either String DexFile
deserializeDex bs =
  let getKnot = snd . either error id
      res = S.runGet (getDex (getKnot res)) bs
  in case res of
    Left err -> Left err
    Right (df, _) -> Right df

data Knot = Knot { knotTypeTable :: Map Int Type
                 , knotClasses :: Map Int Class
                 , knotMethods :: Map Int Method
                 , knotValues :: Map Int Value
                 }

emptyKnot :: Knot
emptyKnot = Knot { knotTypeTable = M.empty
                 , knotClasses = M.empty
                 , knotMethods = M.empty
                 , knotValues = M.empty
                 }

serializeDex :: DexFile -> BS.ByteString
serializeDex = S.runPut . putDex

-- | Format:
--
-- Int (id source)
-- Type list
-- Constant list
-- Class list
putDex :: DexFile -> S.Put
putDex df = do
  S.put (dexIdSrc df)
  tt <- putTypeTable (dexTypes df)
  S.putListOf (putConstant tt) (dexConstants df)
  S.putWord64le (fromIntegral (length (dexClasses df)))
  mapM_ (putClass tt) (dexClasses df)

getDex :: Knot -> S.Get (DexFile, Knot)
getDex fknot = do
  idSrc <- S.get
  tt <- getTypeTable
  constants <- S.getListOf (getConstant tt)
  let knot0 = emptyKnot { knotTypeTable = tt
                        , knotValues = foldr (\c -> M.insert (constantId c) (toValue c)) M.empty constants
                        }
  (classes, knot1) <- getList (getClass fknot) knot0
  let cache = foldr (\klass -> HM.insert (classType klass) klass) HM.empty classes
  return (DexFile { dexClasses = reverse classes
                  , dexConstants = reverse constants
                  , dexTypes = M.elems tt
                  , dexIdSrc = idSrc
                  , _dexClassesByType = cache
                  }, knot1)

getTypeTable :: S.Get (Map Int Type)
getTypeTable = do
  lst <- S.get
  return $ foldr (\(i, t) -> M.insert i t) M.empty lst

putTypeTable :: [Type] -> S.PutM (Map Type Int)
putTypeTable (zip [0..] -> ts) = do
  let m = foldr (\(i, t) -> M.insert t i) M.empty ts
  S.put ts
  return m

putConstant :: Map Type Int -> Constant -> S.Put
putConstant tt c =
  case c of
    ConstantInt uid i -> do
      S.putWord8 0
      S.put uid
      S.put i
    ConstantString uid s -> do
      S.putWord8 1
      S.put uid
      S.put s
    ConstantClass uid t -> do
      S.putWord8 2
      S.put uid
      putType tt t

getConstant :: Map Int Type -> S.Get Constant
getConstant tt = do
  tag <- S.getWord8
  case tag of
    0 -> ConstantInt <$> S.get <*> S.get
    1 -> ConstantString <$> S.get <*> S.get
    2 -> do
      uid <- S.get
      t <- getType tt
      return $ ConstantClass uid t
    _ -> error ("Deserializing invalid constant tag: " ++ show tag)

putType :: Map Type Int -> Type -> S.Put
putType tt t =
  case M.lookup t tt of
    Just tid -> S.put tid
    Nothing -> error ("Serializing type with no id: " ++ show t)

getType :: Map Int Type -> S.Get Type
getType tt = do
  tid <- S.get
  case M.lookup tid tt of
    Just t -> return t
    Nothing -> error ("Deserializing type with unknown id: " ++ show tid)

putClass :: Map Type Int -> Class -> S.Put
putClass tt klass = do
  S.put (classId klass)
  putType tt (classType klass)
  S.put (className klass)
  S.put (classSourceName klass)
  S.put (classAccessFlags klass)
  S.putMaybeOf (putType tt) (classParent klass)
  S.putMaybeOf S.put (fmap classId (classParentReference klass))
  S.putListOf (putType tt) (classInterfaces klass)
  putList (putMethod tt) (classDirectMethods klass)
  putList (putMethod tt) (classVirtualMethods klass)
  S.putListOf (putAccessField tt) (classStaticFields klass)
  S.putListOf (putAccessField tt) (classInstanceFields klass)

getClass :: Knot -> Knot -> S.Get (Class, Knot)
getClass fknot k0 = do
  let tt = knotTypeTable k0
  cid <- S.get
  ct <- getType tt
  name <- S.get
  sourceName <- S.get
  flags <- S.get
  parent <- S.getMaybeOf (getType tt)
  parentRefId <- S.get
  let parentRef = M.lookup parentRefId (knotClasses fknot)
  ifaces <- S.getListOf (getType tt)
  (dms, k1) <- getList (getMethod fknot) k0
  (vms, k2) <- getList (getMethod fknot) k1
  sfields <- S.getListOf (getAccessField tt)
  ifields <- S.getListOf (getAccessField tt)
  let klass = Class { classId = cid
                    , classType = ct
                    , className = name
                    , classSourceName = sourceName
                    , classAccessFlags = flags
                    , classParent = parent
                    , classParentReference = parentRef
                    , classInterfaces = reverse ifaces
                    , classDirectMethods = dms
                    , classVirtualMethods = vms
                    , classStaticFields = reverse sfields
                    , classInstanceFields = reverse ifields
                    , _classStaticFieldMap = indexFields sfields
                    , _classInstanceFieldMap = indexFields ifields
                    }
  return (klass, k2)
  where
    indexFields = foldr (\(_, f) -> HM.insert (fieldName f) f) HM.empty

putAccessField :: Map Type Int -> (AccessFlags, Field) -> S.Put
putAccessField tt (flags, f) = S.put flags >> putField tt f

getAccessField :: Map Int Type -> S.Get (AccessFlags, Field)
getAccessField tt = do
  flags <- S.get
  f <- getField tt
  return (flags, f)

putField :: Map Type Int -> Field -> S.Put
putField tt f = do
  S.put (fieldId f)
  S.put (fieldName f)
  putType tt (fieldType f)
  putType tt (fieldClass f)

getField :: Map Int Type -> S.Get Field
getField tt = do
  fid <- S.get
  name <- S.get
  ft <- getType tt
  fc <- getType tt
  return Field { fieldId = fid
               , fieldName = name
               , fieldType = ft
               , fieldClass = fc
               }

putMethod :: Map Type Int -> Method -> S.Put
putMethod tt m = do
  S.put (methodId m)
  S.put (methodName m)
  putType tt (methodReturnType m)
  S.put (methodAccessFlags m)
  putList (putParameter tt) (methodParameters m)
  S.putMaybeOf (putList (putBlock tt)) (methodBody m)
  S.put (classId (methodClass m))

getMethod :: Knot -> Knot -> S.Get (Method, Knot)
getMethod = undefined

putParameter :: Map Type Int -> Parameter -> S.Put
putParameter tt p = do
  S.put (parameterId p)
  putType tt (parameterType p)
  S.put (parameterName p)
  S.put (parameterIndex p)
  S.put (methodId (parameterMethod p))


getParameter :: Knot -> Knot -> S.Get (Parameter, Knot)
getParameter fknot k = do
  pid <- S.get
  pt <- getType (knotTypeTable k)
  name <- S.get
  ix <- S.get
  mix <- S.get
  let errMsg = error ("No method " ++ show mix ++ " for parameter " ++ show pid)
      m = fromMaybe errMsg $ M.lookup mix (knotMethods fknot)
      p = Parameter { parameterId = pid
                    , parameterType = pt
                    , parameterName = name
                    , parameterIndex = ix
                    , parameterMethod = m
                    }
      k' = k { knotValues = M.insert pid (toValue p) (knotValues k) }
  return (p, k')

putBlock :: Map Type Int -> BasicBlock -> S.Put
putBlock = undefined



putList :: (a -> S.PutM ()) -> [a] -> S.Put
putList p l = do
  S.putWord64le (fromIntegral (length l))
  mapM_ p l

getList :: (Knot -> S.Get (a, Knot)) -> Knot -> S.Get ([a], Knot)
getList p knot0 = S.getWord64le >>= go ([], knot0)
  where
    go (lst, k) 0 = return (reverse lst, k)
    go (lst, k) !n = do
      (elt, k') <- p k
      go (elt : lst, k') (n - 1)
