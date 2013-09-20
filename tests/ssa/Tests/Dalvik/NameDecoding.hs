{-# LANGUAGE OverloadedStrings #-}
module Tests.Dalvik.NameDecoding ( tests ) where

import Data.ByteString ( ByteString )
import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import qualified Test.HUnit as T

import Dalvik.ClassHierarchy
import Dalvik.SSA.Internal.Names
import Dalvik.SSA.Types

tests :: Test
tests = T.testGroup "Java name decoding tests" $ map makeTest
   [ ("V", VoidType)
   , ("Z", BooleanType)
   , ("B", ByteType)
   , ("S", ShortType)
   , ("I", IntType)
   , ("J", LongType)
   , ("F", FloatType)
   , ("D", DoubleType)
   , ("[I", ArrayType IntType)
   , ("[[D", ArrayType (ArrayType DoubleType))
   , ("Ljava/lang/Object;", ReferenceType (qualifiedClassName ["java", "lang"] "Object"))
   ]

makeTest :: (ByteString, Type) -> Test
makeTest (s, oracle) = testCase ("Decode " ++ show s) $
  let Just res = parseTypeName s
  in T.assertEqual "Mismatch" oracle res
