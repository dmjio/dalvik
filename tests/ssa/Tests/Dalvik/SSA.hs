{-# LANGUAGE OverloadedStrings #-}
module Tests.Dalvik.SSA where

import qualified Data.ByteString as BS
import Data.Word (Word16)
import qualified Dalvik.Types as DT
import Dalvik.SSA (methodRegisterAssignment)

import Tests.Dalvik.DexLoaders (readAsDex, getEncodedMethod)

import System.FilePath ((</>))

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import Test.HUnit (Assertion, assertFailure, (@=?))

javaInputs :: FilePath
javaInputs = "./tests/testfiles/javaInputs"


tests :: Test
tests = T.testGroup "SSA tests" $ [
         T.testGroup "Method Register Assignment" $ map methRegTests
              [ ( "LTest;", "intLongAdd", "(IJ)J" -- static method.
                , javaInputs </> "Test.java"
                , Just [(Nothing,2), (Nothing,3)] )

              , ( "LTest;", "staticNop", "()V"  -- static method, no params.
                , javaInputs </> "Test.java"
                , Just [] )

              , ( "LTest;", "takeDouble", "(IDC)V" -- static method.
                , javaInputs </> "Test.java"
                , Just [(Nothing, 0), (Nothing, 1), (Nothing, 3)] )

              , ( "LTest;", "nop", "()V"
                , javaInputs </> "Test.java"
                , Just [(Just "this", 0)] )

              , ( "LTest;", "echoNop", "()V" -- I think this uses a register for the println (?)
                , javaInputs </> "Test.java"
                , Just [(Just "this", 1)] )

              , ( "LTest;", "stringID", "(Ljava/lang/String;)Ljava/lang/Object;"
                , javaInputs </> "Test.java"
                , Just [(Just "this",0), (Nothing,1)] )

              , ( "LTest;", "voidFn", "(Ljava/lang/String;LTest;)V"
                , javaInputs </> "Test.java"
                , Just [(Just "this",0), (Nothing,1), (Nothing,2)] )

              , ( "LTest$InnerTest;", "stringID", "(Ljava/lang/String;)Ljava/lang/Object;"
                , javaInputs </> "Test.java"
                , Just [(Just "this",0), (Nothing,1)] )
              ]
        , T.testGroup "getEncodedMethod" $ map findEncMethodTest
              [ ( "LTest;", "intLongAdd", "(IJ)J"
                , javaInputs </> "Test.java" )

              , ( "LTest;", "nop", "()V"
                , javaInputs </> "Test.java")

              , ( "LTest;", "stringID", "(Ljava/lang/String;)Ljava/lang/Object;"
                , javaInputs </> "Test.java")

              , ( "LTest;", "voidFn", "(Ljava/lang/String;LTest;)V"
                , javaInputs </> "Test.java")

              , ( "LTest$InnerTest;", "voidFn", "(Ljava/lang/String;LTest;)V"
                , javaInputs </> "Test.java")

              , ( "LTest$InnerTest;", "innerClassParam", "(LTest$InnerTest;)V"
                , javaInputs </> "Test.java")
              ]
        ]

methRegTests :: (String, String, String, FilePath, Maybe [(Maybe BS.ByteString, Word16)]) -> Test
methRegTests (clas, method, sig, file, oracle) =
  testWithDexFile ("Arg assignments: " ++ toStr clas method sig) file $ \dexFile ->
    case getEncodedMethod dexFile clas method sig of
      Nothing -> assertFailure ("Could not find method: "++toStr clas method sig)
      Just  m -> oracle @=? methodRegisterAssignment dexFile m

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
