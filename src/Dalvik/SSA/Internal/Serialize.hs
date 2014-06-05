{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dalvik.SSA.Internal.Serialize () where


import qualified Data.Serialize as S

import Dalvik.SSA.Types

instance S.Serialize DexFile where
  put = putDex
  get = getDex

-- | Start off with a table of types and a table of constants.  These
-- will be referred to later by index to preserve sharing.  These two
-- tables are followed by the unique ID counter.
putDex :: DexFile -> S.Put
putDex = undefined

getDex :: S.Get DexFile
getDex = undefined
