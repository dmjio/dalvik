{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Dalvik.ClassHierarchy (
  -- * Class Hierarchy Analysis
  CHA,
  classHierarchyAnalysis,
  classHierarchyParent
  ) where

import Control.Failure
import Control.Monad ( foldM, liftM )
import qualified Data.ByteString.Char8 as BS
import Data.Map
import qualified Data.Map as M

import Dalvik.ClassName
import Dalvik.Types

-- | The result of a class hierarchy analysis
data CHA = CHA (Map BS.ByteString BS.ByteString)
         deriving (Eq, Ord, Show)

-- | Extract the class hierarchy from a Dex file.  This hierarchy only
-- includes the types referenced in the Dex file.
classHierarchyAnalysis :: (Failure DecodeError f) => DexFile -> f CHA
classHierarchyAnalysis dex =
  liftM CHA $ foldM addClassParent M.empty $ M.toList (dexClasses dex)
  where
    addClassParent m (tyId, klass) = do
      cname <- getTypeName dex tyId
      case cname == "Ljava/lang/Object;" of
        True -> return m
        False -> do
          superName <- getTypeName dex (classSuperId klass)
          return $ M.insert cname superName m

-- | Look up the parent of a class.  Only Object has no parent.
--
-- The input is a 'NameLike': either a ByteString or a ClassName
-- (which is easily convertible to ByteString).
classHierarchyParent :: (NameLike a) => CHA -> a -> Maybe BS.ByteString
classHierarchyParent (CHA cha) cname = M.lookup (toName cname) cha

class NameLike a where
  toName :: a -> BS.ByteString

instance NameLike BS.ByteString where
  toName = id

instance NameLike ClassName where
  toName = renderClassName
