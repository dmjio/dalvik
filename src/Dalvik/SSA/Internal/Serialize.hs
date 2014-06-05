{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dalvik.SSA.Internal.Serialize () where

import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Serialize as S

import Dalvik.SSA.Types

instance S.Serialize DexFile where
  put = putDex
  get = getDex

instance S.Serialize Type

-- | Start off with a table of types and a table of constants.  These
-- will be referred to later by index to preserve sharing.  These two
-- tables are followed by the unique ID counter.

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
  S.putListOf (putClass tt) (dexClasses df)

getDex :: S.Get DexFile
getDex = do
  idSrc <- S.get
  tt <- getTypeTable
  constants <- S.getListOf (getConstant tt)
  classes <- S.getListOf (getClass tt)
  let cache = foldr (\klass -> HM.insert (classType klass) klass) HM.empty classes
  return DexFile { dexClasses = classes
                 , dexConstants = constants
                 , dexTypes = M.elems tt
                 , dexIdSrc = idSrc
                 , _dexClassesByType = cache
                 }

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
      tid <- S.get
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
  S.putListOf (putMethod tt) (classDirectMethods klass)
  S.putListOf (putMethod tt) (classVirtualMethods klass)
  S.putListOf (putAccessField tt) (classStaticFields klass)
  S.putListOf (putAccessField tt) (classInstanceFields klass)

getClass :: Map Int Type -> S.Get Class
getClass tt = do
  cid <- S.get
  ct <- getType tt
  name <- S.get
  sourceName <- S.get
  flags <- S.get
  parent <- S.getMaybeOf getType
  parentRefId <- S.get
  ifaces <- S.getListOf (getType tt)
  dms <- S.getListOf (getMethod tt)
  return undefined

putAccessField :: Map Type Int -> (AccessFlags, Field) -> S.Put
putAccessField tt (flags, f) = S.put flags >> putField tt f

getAccessField :: Map Int Type -> S.Get (AccessFlags, Field)
getAccessField tt = do
  flags <- S.get
  f <- getField tt
  return (flags, f)

putField :: Map Type Int -> Field -> S.Put
putField tt f = undefined

getField :: Map Int Type -> S.Get Field
getField = undefined

putMethod :: Map Type Int -> Method -> S.Put
putMethod = undefined

getMethod :: Map Int Type -> S.Get Method
getMethod = undefined
