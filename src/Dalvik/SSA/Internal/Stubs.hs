-- | Define stub methods to override real definitions
module Dalvik.SSA.Internal.Stubs (
  Stubs,
  stubs,
  matchingStub
  ) where

import Control.Monad ( guard )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )

import qualified Dalvik.Types as DT

type Sig = ([ByteString], ByteString)
type Descriptor = (ByteString, ByteString, Sig)

data Stubs = Stubs { stubDexFile :: DT.DexFile
                   , stubMap :: HashMap Descriptor DT.EncodedMethod
                   }
           deriving (Show)

-- | Given a 'DT.DexFile' (non-SSA form) filled with stub definitions,
-- build an abstract representation that can be used to merge them
-- into a larger dex file (overwriting the original definitions).
--
-- We require a prefix on all of our stub definitions because the
-- compiler would not allow core classes to be overwritten.
--
-- Stubs introduced into an SSA 'Dalvik.SSA.Types.DexFile' override
-- the definitions found in the input dex files.  These are most
-- useful for providing stub implementations of native methods.
--
-- Note:
--
--  1) ANY normal method definition can be overridden.
--
--  2) You cannot have constructor stubs (for <init> functions).  This
-- restriction is in place because you can't (?) suppress the
-- generation of default constructors, and letting them override the
-- real default constructors would be dangerous.
--
-- Stubs are provided through an additional 'DT.DexFile'.  This input
-- should mirror exactly the class hierarchy to be overridden, except
-- that each class should have a package prefix (also specified as an
-- input.
--
-- For example, if you wanted to provide a stub implementation of
-- java.lang.String.intern (which is implemented as native code), you
-- would create a java file like the following:
--
-- > package stubs.java.lang;
-- > public class String {
-- >   String intern() {
-- >     return this;
-- >   }
-- > }
--
-- Then compile it into a dex file:
--
-- > javac stubs/java/lang/String.java
-- > jar cf Stubs.jar stubs/java/lang/String.class
-- > dx --dex --output=Stubs.dex Stubs.jar
--
-- With this stub file, you could overwrite existing definitions
-- (from, say, jvm.dex if you had one):
--
-- > Right dx <- loadDexFromAnyIO "jvm.dex"
-- > Right stubDex <- loadDexFromAnyIO "Stubs.dex"
-- > ssa <- toSSA (Just (stubs "stubs." stubDex)) [dx]
--
-- This merged SSA-form dex file will have a simple definition of
-- @intern@ instead of a NATIVE tag.
stubs :: ByteString -- ^ Prefix
         -> DT.DexFile -- ^ A dex file with stub implementations
         -> Stubs
stubs pfx df =
  Stubs { stubDexFile = df'
        , stubMap = F.foldl' (addClassMethods df') HM.empty (DT.dexClasses df')
        }
  where
    -- We specify prefixes with dotted names to make it simpler to
    -- think about for callers.  The names are actually stored with
    -- slashes, though, so replace them here.
    tpfx = BS.map (\c -> if c == '.' then '/' else c) pfx
    -- All of our custom types will have prefixes since they have to
    -- be in their own packages.  Strip off the prefixes brute force.
    -- They are all in the strings table, so this should catch
    -- everything.
    df' = df { DT.dexStrings = fmap (stripPrefix tpfx) (DT.dexStrings df) }

stripPrefix :: ByteString -> ByteString -> ByteString
stripPrefix pfx s
  | BS.isInfixOf pfx s =
    let (start, rest) = BS.breakSubstring pfx s
    in BS.concat [start, BS.drop (BS.length pfx) rest]
  | otherwise = s

matchingStub :: Stubs -> DT.DexFile -> DT.EncodedMethod -> Maybe (DT.DexFile, DT.EncodedMethod)
matchingStub st context encMethod = do
  method <- DT.getMethod context (DT.methId encMethod)
  mname <- DT.getStr context (DT.methNameId method)
  guard (mname /= fromString "<init>")
  msig <- DT.encodedMethodSignature context encMethod
  cname <- DT.getTypeName context (DT.methClassId method)
  em <- HM.lookup (cname, mname, msig) (stubMap st)
  return (stubDexFile st, em)

addClassMethods :: DT.DexFile
                   -> HashMap Descriptor DT.EncodedMethod
                   -> DT.Class
                   -> HashMap Descriptor DT.EncodedMethod
addClassMethods df m klass = fromMaybe m $ do
  cname <- DT.getTypeName df (DT.classId klass)
  return $ F.foldl' (addStubMethod df cname) m ms
  where
    ms = DT.classDirectMethods klass ++ DT.classVirtualMethods klass

addStubMethod :: DT.DexFile
                 -> ByteString
                 -> HashMap Descriptor DT.EncodedMethod
                 -> DT.EncodedMethod
                 -> HashMap Descriptor DT.EncodedMethod
addStubMethod df cname m encMethod = fromMaybe m $ do
  method <- DT.getMethod df (DT.methId encMethod)
  mname <- DT.getStr df (DT.methNameId method)
  sig <- DT.encodedMethodSignature df encMethod
  return $ HM.insert (cname, mname, sig) encMethod m
