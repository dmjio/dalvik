{-# LANGUAGE ViewPatterns #-}
module Dalvik.SSA.Internal.Serialize (
  deserializeDex,
  serializeDex
  ) where

import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Data.Map ( Map )
import qualified Data.Map as M
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
                 , knotValues :: Map Int Value
                 }

emptyKnot :: Knot
emptyKnot = Knot { knotTypeTable = M.empty
                 , knotClasses = M.empty
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
  nClasses <- S.getWord64le
  (classes, knot1) <- F.foldlM (getClass fknot) ([], knot0) [0..nClasses - 1]
  let cache = foldr (\klass -> HM.insert (classType klass) klass) HM.empty classes
  return (DexFile { dexClasses = classes
                  , dexConstants = constants
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
  S.putWord64le (fromIntegral (length (classDirectMethods klass)))
  mapM_ (putMethod tt) (classDirectMethods klass)
  S.putWord64le (fromIntegral (length (classVirtualMethods klass)))
  mapM_ (putMethod tt) (classVirtualMethods klass)
  S.putListOf (putAccessField tt) (classStaticFields klass)
  S.putListOf (putAccessField tt) (classInstanceFields klass)

getClass :: Knot -> ([Class], Knot) -> a -> S.Get ([Class], Knot)
getClass fknot (classes, k0) _ = do
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
  nStatics <- S.getWord64le
  (dms, k1) <- F.foldlM (getMethod fknot) ([], k0) [0..nStatics - 1]
  nVirts <- S.getWord64le
  (vms, k2) <- F.foldlM (getMethod fknot) ([], k1) [0..nVirts - 1]
  sfields <- S.getListOf (getAccessField tt)
  ifields <- S.getListOf (getAccessField tt)
  let klass = Class { classId = cid
                    , classType = ct
                    , className = name
                    , classSourceName = sourceName
                    , classAccessFlags = flags
                    , classParent = parent
                    , classParentReference = parentRef
                    , classInterfaces = ifaces
                    , classDirectMethods = dms
                    , classVirtualMethods = vms
                    , classStaticFields = sfields
                    , classInstanceFields = ifields
                    , _classStaticFieldMap = indexFields sfields
                    , _classInstanceFieldMap = indexFields ifields
                    }
  return (klass : classes, k2)
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
putMethod = undefined

getMethod :: Knot -> ([Method], Knot) -> a -> S.Get ([Method], Knot)
getMethod = undefined
