{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Dalvik.SSA.Util (
  findClassByName,
  findMethodByName,
  findMethodByNameString,
  findInstanceFieldByName,
  findStaticFieldByName,
  resolveStaticField,
  stripCasts,
  LookupError(..)
  ) where

import qualified Control.Monad.Catch as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe ( fromMaybe )
import Data.Typeable

import qualified Dalvik.SSA.ClassHierarchy as CHA
import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Types

-- | Strip any cast instructions off of the given 'Value'
stripCasts :: (IsValue a) => a -> Value
stripCasts (toValue -> v) = fromMaybe v $ do
  CheckCast { castReference = r } <- fromValue v
  return (stripCasts r)

data LookupError = NoClassFound BS.ByteString
                 | NoMethodFound BS.ByteString String BS.ByteString
                 | NoFieldFound BS.ByteString BS.ByteString
                 deriving (Eq, Ord, Show, Typeable)

instance E.Exception LookupError

findClassByName :: (E.MonadThrow m) => BS.ByteString -> DexFile -> m Class
findClassByName name df = maybe err return $ do
  HM.lookup name (_dexClassesByName df)
  where
    err = E.throwM $ NoClassFound name

findMethodByName :: (E.MonadThrow m) => BS.ByteString -> MethodSignature -> Class -> m Method
findMethodByName mname sig klass = maybe err return $ do
  HM.lookup (mname, sig) (_classMethodMap klass)
  where
    err = E.throwM $ NoMethodFound mname (show sig) (className klass)

findMethodByNameString :: (E.MonadThrow m) => BS.ByteString -> String -> Class -> m Method
findMethodByNameString name ssig klass = do
  sig <- parseMethodSignature (BS.pack ssig)
  findMethodByName name sig klass

findStaticFieldByName :: (E.MonadThrow m) => BS.ByteString -> Class -> m Field
findStaticFieldByName name klass = maybe err (return . snd) $ do
  L.find ((== name) . fieldName . snd) (classStaticFields klass)
  where
    err = E.throwM $ NoFieldFound name (className klass)

findInstanceFieldByName :: (E.MonadThrow m) => BS.ByteString -> Class -> m Field
findInstanceFieldByName name klass = maybe err (return . snd) $ do
  L.find ((== name) . fieldName . snd) (classInstanceFields klass)
  where
    err = E.throwM $ NoFieldFound name (className klass)

-- | Static field references in the Java source (and bytecode) do not
-- necessarily refer to the class containing that static field.  For example
--
-- > F.bar
--
-- refers to the @bar@ field in the parent class of @F@ if @F@ does
-- not have its own declaration of @bar@.  This method resolves static
-- field references to the real underlying field.
resolveStaticField :: CHA.ClassHierarchy -> Field -> Field
resolveStaticField cha f0 = fromMaybe f0 (go (fieldClass f0))
  where
    go t = do
      dfn <- CHA.definition cha t
      case classStaticField dfn (fieldName f0) of
        Just f -> return f
        Nothing -> classParent dfn >>= go
