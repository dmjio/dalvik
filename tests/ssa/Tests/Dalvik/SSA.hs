module Tests.Dalvik.SSA where

import Data.Word (Word16)
import qualified Dalvik.Types as DT
import Dalvik.SSA (methodRegisterAssignment)

import Tests.Dalvik.DexLoaders (readAsDex, getEncodedMethod)

import System.FilePath ((</>))

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import Test.HUnit (Assertion, assertFailure, (@=?))

javaInputs = "./tests/testfiles/javaInputs"


tests :: Test
tests = T.testGroup "SSA tests" $ [
         T.testGroup "Method Register Assignment" $ map methRegTests
              [ ("intLongAdd(IJ)J arg offests"
                , javaInputs </> "Test.java"
                , (DT.EncodedMethod 1 0 Nothing) -- needs updated to use getEncodedMethod
                , Just [(Nothing,3), (Nothing,2)] )
              ]
        , T.testGroup "getEncodedMethod" $ map findEncMethodTest
              [ ( "LTest;", "intLongAdd", "JIJ"
                , javaInputs </> "Test.java" )

              , ( "LTest;", "stringId", "Ljava/lang/Object;Ljava/lang/String;"
                , javaInputs </> "Test.java")

              , ( "LTest;", "voidFn", "VLjava/lang/String;LTest;"
                , javaInputs </> "Test.java")

              , ( "LTest$InnerTest;", "voidFn", "VLjava/lang/String;LTest;"
                , javaInputs </> "Test.java")

              , ( "LTest$InnerTest;", "innerClassParam", "VLTest$InnerTest;"
                , javaInputs </> "Test.java")
              ]
        ]

methRegTests :: (String, FilePath, DT.EncodedMethod, Maybe [(Maybe String, Word16)]) -> Test
methRegTests (descr, file, mth, oracle) =
  testWithDexFile descr file $ \dexFile ->
    oracle @=? methodRegisterAssignment dexFile mth

findEncMethodTest :: (String, FilePath, String, String) -> Test
findEncMethodTest (clas, method, sig, file) =
  testWithDexFile ("Find " ++ toStr clas method sig) file $ \dexFile ->
    case getEncodedMethod dexFile clas method sig of
      Nothing -> assertFailure ("Could not find method: "++toStr clas method sig)
      Just _  -> return ()

toStr :: String -> String -> String -> String
toStr c m sig = c ++ m ++ "(" ++ sig ++ ")"

testWithDexFile :: String -> FilePath -> (DT.DexFile -> Assertion) -> Test
testWithDexFile descr file fn = T.testCase descr $ do
  eDexFile <- readAsDex file
  case eDexFile of
    Left err      -> assertFailure err
    Right dexFile -> fn dexFile