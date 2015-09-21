{-# LANGUAGE OverloadedStrings #-}
module Tests.Dalvik.SSA ( tests ) where

import qualified Data.ByteString as BS
import Data.Word (Word16)
import Dalvik.Apk
import qualified Dalvik.Types as DT
import Dalvik.SSA.Internal.RegisterAssignment

import System.FilePath ((</>))
import Text.Printf (printf)

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import Test.HUnit (Assertion, assertFailure, (@=?))

javaInputs :: FilePath
javaInputs = "./tests/testfiles/javaInputs"

tests :: Test
tests = T.buildTest $ do
  getDex <- memoIO loadDexFromAnyIO
  return $ T.testGroup "SSA tests" $ [
         T.testGroup "Method Register Assignment" $ map (getParamListTests getDex)
              [ ( "LTest;", "intLongAdd", "(IJ)J" -- static method.
                , javaInputs </> "Test.java"
                , Just [(Nothing,"I"), (Nothing,"J")] )

              , ( "LTest;", "staticNop", "()V"  -- static method, no params.
                , javaInputs </> "Test.java"
                , Just [] )

              , ( "LTest;", "nop", "()V"
                , javaInputs </> "Test.java"
                , Just [(Just "this", "LTest;")] )

              , ( "LTest;", "stringID", "(Ljava/lang/String;)Ljava/lang/Object;"
                , javaInputs </> "Test.java"
                , Just [(Just "this","LTest;"), (Nothing, "Ljava/lang/String;")] )
              ]
         , T.testGroup "Method Register Assignment" $ map (methRegTests getDex)
              [ ( "LTest;", "intLongAdd", "(IJ)J" -- static method.
                , javaInputs </> "Test.java"
                , Just [(Nothing,3), (Nothing,4)] )

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
        , T.testGroup "getEncodedMethod" $ map (findEncMethodTest getDex)
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

getParamListTests :: DexReader -> (String, String, String, FilePath, Maybe [(Maybe BS.ByteString, BS.ByteString)]) -> Test
getParamListTests getDex (clas, method, sig, file, oracle) =
  testWithDexFile getDex ("getParamListTypeNames: " ++ toStr clas method sig) file $ \dexFile ->
    case DT.getEncodedMethod dexFile clas method sig of
      Nothing -> assertFailure ("Could not find method: "++toStr clas method sig)
      Just  m -> oracle @=? getParamListTypeNames dexFile m


methRegTests :: DexReader -> (String, String, String, FilePath, Maybe [(Maybe BS.ByteString, Word16)]) -> Test
methRegTests getDex (clas, method, sig, file, oracle) =
  testWithDexFile getDex ("Arg assignments: " ++ toStr clas method sig) file $ \dexFile ->
    case DT.getEncodedMethod dexFile clas method sig of
      Nothing -> assertFailure ("Could not find method: "++toStr clas method sig)
      Just  m -> oracle @=? methodRegisterAssignment dexFile m

findEncMethodTest :: DexReader -> (String, FilePath, String, String) -> Test
findEncMethodTest getDex (clas, method, sig, file) =
  testWithDexFile getDex ("Find " ++ toStr clas method sig) file $ \dexFile ->
    case DT.getEncodedMethod dexFile clas method sig of
      Nothing -> assertFailure ("Could not find method: "++toStr clas method sig)
      Just _  -> return ()

toStr :: String -> String -> String -> String
toStr []      m sig = printf "%s%s" m sig
toStr c@(_:_) m sig | last c /= ';' = printf "%s%s%s" c m sig
                    | otherwise     = let
                      pkgName = (init c) ++ "."
                      in printf "%s%s%s" pkgName m sig

testWithDexFile :: DexReader -> String -> FilePath -> (DT.DexFile -> Assertion) -> Test
testWithDexFile getDex descr file fn = T.testCase descr $ do
  eDexFile <- getDex file
  case eDexFile of
    Left err      -> assertFailure err
    Right dexFile -> fn dexFile
