{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module defines tests for the SSA labeling code.
--
-- Each test description describes the method to label (class, method,
-- signature) and expected results of the labeling.  Each method is
-- analyzed individually, with SSA value labels being assigned to each
-- value.  The expected result of each test is the label of the value
-- returned by the return instruction.  If the returned label is a phi
-- label, we additionally check to make sure that the values referenced by
-- that phi node are correct (by checking thee labels).  Only one level of
-- phi label checking is done - we don't recursively check everything.
module Tests.Dalvik.Labels ( tests ) where

import qualified Control.Exception as E
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S

import Dalvik.Apk
import Dalvik.Instruction
import qualified Dalvik.Types as DT
import Dalvik.SSA.Internal.Labeling

import System.FilePath ((</>))
import Text.Printf (printf)

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import qualified Test.HUnit as T

javaInputs :: FilePath
javaInputs = "./tests/testfiles/javaInputs"

tests :: Test
tests = T.buildTest $ do
  getDex <- memoIO loadDexFromAnyIO
  return $ T.testGroup "SSA Label tests" $ map (getLabelTests getDex (javaInputs </> "LabelTests.java"))
    [ ("LLabelTests;", "localCopies", "(II)I", ArgumentLabel "%arg1" 1, [])
    , ("LLabelTests;", "simpleBranch", "(II)I", PhiLabel 1 [0,2] 7,
       [ArgumentLabel "%arg1" 1,ArgumentLabel "%arg2" 2])
    , ("LLabelTests;", "simplePackedSwitch", "(III)I", PhiLabel 2 [0,1,3] 9,
       [SimpleLabel 8,ArgumentLabel "%arg2" 2,ArgumentLabel "%arg3" 3])
    , ("LLabelTests;", "simpleSparseSwitch", "(III)I", PhiLabel 2 [0,1,3] 9,
       [SimpleLabel 8,ArgumentLabel "%arg2" 2,ArgumentLabel "%arg3" 3])
    , ("LLabelTests;", "simpleLoop", "(II)I", PhiLabel 1 [0,2] 8,
       [SimpleLabel 9,ArgumentLabel "%arg1" 1])
    , ("LLabelTests;", "loopNopBody", "(II)I", ArgumentLabel "%arg1" 1, [])
    , ("LLabelTests;", "nestedLoops", "(II)I", PhiLabel 1 [0,5] 10,
       [SimpleLabel 17, ArgumentLabel "%arg1" 1])
    , ("LLabelTests;", "uncaughtNPE", "([Ljava/lang/String;)I", SimpleLabel 4, [])
    , ("LLabelTests;", "simpleNPECatchNPE", "([Ljava/lang/String;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "simpleNPECatchRuntimeEx", "([Ljava/lang/String;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "simpleNPECatchException", "([Ljava/lang/String;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "simpleNPECatchThrowable", "([Ljava/lang/String;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "simpleNPECatchArithEx", "([Ljava/lang/String;)I", SimpleLabel 4, [])
    , ("LLabelTests;", "simpleNPEOnlyFirstHandler", "([Ljava/lang/String;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "simpleNPEWithHandlerAndFinally", "([Ljava/lang/String;)I", SimpleLabel 4, [])
    , ("LLabelTests;", "divideNoCatch", "(II)I", SimpleLabel 6, [])
    , ("LLabelTests;", "arithNoDivision", "(II)I", SimpleLabel 9, [])
    , ("LLabelTests;", "safeDivideWithCatch", "(II)I", PhiLabel 2 [1,4,5] 7,
       [SimpleLabel 6, SimpleLabel 8, SimpleLabel 10])
    , ("LLabelTests;", "divisionCatchArithEx", "(II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "divisionCatchRuntimeEx", "(II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "divisionCatchException", "(II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "divisionCatchThrowable", "(II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "divisionCatchNPE", "(II)I", SimpleLabel 6, [])
    , ("LLabelTests;", "checkCastNoHandler", "(Ljava/lang/Object;)I", SimpleLabel 5, [])
    , ("LLabelTests;", "checkCastHandleCCE", "(Ljava/lang/Object;)I", PhiLabel 2 [1,3] 6,
       [SimpleLabel 5, SimpleLabel 8])
    , ("LLabelTests;", "checkCastHandleRuntimeException", "(Ljava/lang/Object;)I", PhiLabel 2 [1,3] 6,
       [SimpleLabel 5, SimpleLabel 8])
    , ("LLabelTests;", "checkCastHandleException", "(Ljava/lang/Object;)I", PhiLabel 2 [1,3] 6,
       [SimpleLabel 5, SimpleLabel 8])
    , ("LLabelTests;", "checkCastHandleThrowable", "(Ljava/lang/Object;)I", PhiLabel 2 [1,3] 6,
       [SimpleLabel 5, SimpleLabel 8])
    , ("LLabelTests;", "checkCastHandleArithException", "(Ljava/lang/Object;)I", SimpleLabel 5, [])
    , ("LLabelTests;", "invokeToAllHandlers", "(Ljava/lang/Object;)I", PhiLabel 3 [2,4,5,6,7,8] 6,
       [SimpleLabel 5, SimpleLabel 8, SimpleLabel 10, SimpleLabel 12, SimpleLabel 14, SimpleLabel 16])
    , ("LLabelTests;", "returnThisFieldNoHandler", "()I", SimpleLabel 2, [])
      -- Note that, ideally, we could use a bit more static analysis to make this
      -- test fail in a good way (since @this@ can never be NULL).
    , ("LLabelTests;", "returnThisFieldHandler", "()I", PhiLabel 1 [0,2] 3,
       [SimpleLabel 2, SimpleLabel 5])
    , ("LLabelTests;", "returnOtherFieldNoHandler", "(LLabelTests;)I", SimpleLabel 4, [])
    , ("LLabelTests;", "returnOtherFieldHandler", "(LLabelTests;)I", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "arrayReadNoHandler", "([II)I", SimpleLabel 6, [])
    , ("LLabelTests;", "arrayReadHandler", "([II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "arrayReadHandlerThrowable", "([II)I", PhiLabel 1 [0,2] 7,
       [SimpleLabel 6, SimpleLabel 9])
    , ("LLabelTests;", "arrayWriteNoHandler", "([Ljava/lang/Object;Ljava/lang/Object;I)I", ArgumentLabel "%arg3" 3, [])
    , ("LLabelTests;", "arrayWriteHandler", "([Ljava/lang/Object;Ljava/lang/Object;I)I", PhiLabel 1 [0,2,3] 8,
       [SimpleLabel 10, SimpleLabel 12, ArgumentLabel "%arg3" 3])
    , ("LLabelTests;", "newArrayUnchecked", "(I)[D", SimpleLabel 4, [])
    , ("LLabelTests;", "newArrayChecked", "(I)[D", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 8])
    , ("LLabelTests;", "newArrayCheckedThrowable", "(I)[D", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 8])
    , ("LLabelTests;", "newArrayOOMChecked", "(I)[D", PhiLabel 1 [0,2] 5,
       [SimpleLabel 4, SimpleLabel 7])
    , ("LLabelTests;", "newInstanceUnchecked", "()Ljava/lang/String;", SimpleLabel 2, [])
    , ("LLabelTests;", "newInstanceChecked", "()Ljava/lang/String;", PhiLabel 2 [1,3] 4,
       [SimpleLabel 2, SimpleLabel 6])
    , ("LLabelTests;", "newArrayFilledUnchecked", "()[I", SimpleLabel 3, [])
    -- , ("LLabelTests;", "newArrayFilledCheckedNPE", "()Ljava/lang/Object;", PhiLabel 1 [0,2] 4,
    --    [SimpleLabel 3, SimpleLabel 6])
    , ("LLabelTests;", "newMultiArrayUnchecked", "(SS)Ljava/lang/Object;", SimpleLabel 9, [])
    , ("LLabelTests;", "newMultiArrayCheckedOOM", "(SS)Ljava/lang/Object;", PhiLabel 3 [2,4] 10,
       [SimpleLabel 9, SimpleLabel 13])
    ]

isReturn :: Labeling -> Int -> Bool
isReturn l ix =
  case labelingInstructionAt l ix of
    Just (Return _ _) -> True
    _ -> False

checkReturnValue :: Labeling -> Label -> [Label] -> T.Assertion
checkReturnValue l expected phiExpected = fromMaybe (T.assertFailure "No return instruction found") $ do
  (_, m) <- L.find (isReturn l . fst) instMaps
  case M.toList m of
    [(_, label) ] -> return $ do
      T.assertBool "Non-unique value label" (generatedLabelsAreUnique l)
      T.assertEqual "Unexpected label mismatch" expected label
      case label of
        PhiLabel _ _ _ -> do
          -- If we have a phi node, make sure it is referencing the correct values.
          let Just lbls = M.lookup label (labelingPhis l)
          T.assertEqual "Phi value mismatch" (S.fromList phiExpected) lbls
          T.assertBool "Phi label requires more than one incoming value" (length phiExpected > 1)
        _ -> T.assertEqual "There should not be expected values for non-phis" [] phiExpected
    _ -> return $ T.assertFailure "More than one (or no) register mapped for return"
  where
    instMaps = M.toList (labelingReadRegs l)

getLabelTests :: DexReader -> FilePath -> (String, String, String, Label, [Label]) -> Test
getLabelTests getDex file (klass, method, sig, oracle, oracle') =
  testWithDexFile getDex ("CheckReturn: " ++ toStr klass method sig) file $ \dexFile ->
    case DT.getEncodedMethod dexFile klass method sig of
      Nothing -> T.assertFailure ("Could not find method: " ++ toStr klass method sig)
      Just m ->
        case labelMethod dexFile m of
          Left (E.fromException -> Just e) -> T.assertFailure ("Could not label method: " ++ toStr klass method sig ++ " " ++ DT.decodeErrorAsString e)
          Left e -> T.assertFailure ("Unexpected failure: " ++ show e)
          Right lbls -> checkReturnValue lbls oracle oracle'

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
