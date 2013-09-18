{-# LANGUAGE OverloadedStrings #-}
module Tests.Dalvik.Labels ( tests ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Dalvik.Instruction
import qualified Dalvik.Types as DT
import Dalvik.SSA ( labelMethod )
import Dalvik.SSA.Labelling

import Tests.Dalvik.DexLoaders ( DexReader, memoIO, readAsDex, getEncodedMethod )

import System.FilePath ((</>))
import Text.Printf (printf)

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import qualified Test.HUnit as T

javaInputs :: FilePath
javaInputs = "./tests/testfiles/javaInputs"

tests :: Test
tests = T.buildTest $ do
  getDex <- memoIO readAsDex
  return $ T.testGroup "SSA Label tests" $ map (getLabelTests getDex)
    [ ("LLabelTests;", "localCopies", "(II)I",
       javaInputs </> "LabelTests.java",
       ArgumentLabel "%arg1" 1)
    , ("LLabelTests;", "simpleBranch", "(II)I",
       javaInputs </> "LabelTests.java",
       PhiLabel 1 [0,2] 7)
    , ("LLabelTests;", "simpleLoop", "(II)I",
       javaInputs </> "LabelTests.java",
       PhiLabel 1 [0,2] 8)
    , ("LLabelTests;", "loopNopBody", "(II)I",
       javaInputs </> "LabelTests.java",
       ArgumentLabel "%arg1" 1)
    , ("LLabelTests;", "uncaughtNPE", "(Ljava/lang/String;)I",
       javaInputs </> "LabelTests.java",
       SimpleLabel 4)
    ]

isReturn :: Instruction -> Bool
isReturn i =
  case i of
    Return _ _ -> True
    _ -> False

checkReturnValue :: Labelling -> Label -> T.Assertion
checkReturnValue l expected = fromMaybe (T.assertFailure "No return instruction found") $ do
  (_, m) <- L.find (isReturn . fst) instMaps
  case M.toList m of
    [(_, label) ] -> return $ do
      T.assertBool "Non-unique value label" (generatedLabelsAreUnique l)
      T.assertEqual "Unexpected label mismatch" expected label
    _ -> return $ T.assertFailure "More than one (or no) register mapped for return"
  where
    instMaps = M.toList (labellingReadRegs l)

getLabelTests :: DexReader -> (String, String, String, FilePath, Label) -> Test
getLabelTests getDex (klass, method, sig, file, oracle) =
  testWithDexFile getDex ("getLabel: " ++ toStr klass method sig) file $ \dexFile ->
    case getEncodedMethod dexFile klass method sig of
      Nothing -> T.assertFailure ("Could not find method: " ++ toStr klass method sig)
      Just m ->
        case labelMethod dexFile m of
          Left e -> T.assertFailure ("Could not label method: " ++ toStr klass method sig ++ " " ++ DT.decodeErrorAsString e)
          Right lbls -> checkReturnValue lbls oracle

toStr :: String -> String -> String -> String
toStr []      m sig = printf "%s%s" m sig
toStr c@(_:_) m sig | last c /= ';' = printf "%s%s%s" c m sig
                    | otherwise     = let
                      pkgName = (init c) ++ "."
                      in printf "%s%s%s" pkgName m sig

testWithDexFile :: DexReader -> String -> FilePath -> (DT.DexFile -> T.Assertion) -> Test
testWithDexFile getDex descr file fn = T.testCase descr $ do
  eDexFile <- getDex file
  case eDexFile of
    Left err      -> T.assertFailure err
    Right dexFile -> fn dexFile
