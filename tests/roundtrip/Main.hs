module Main ( main ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import qualified Test.HUnit as T
import qualified Dalvik.Apk as Apk
import Dalvik.SSA

main :: IO ()
main = T.defaultMain [tg]
  where
    tg = T.testCase "Roundtrip Test" $ do
      Right core <- Apk.loadDexFromAnyIO "tests/roundtrip/core.dex"
      Right frame <- Apk.loadDexFromAnyIO "tests/roundtrip/framework.dex"
      ssa0 <- toSSA Nothing Nothing [core, frame]
      let Right ssa1 = deserializeDex $ serializeDex ssa0
      deepEq ssa0 ssa1

deepEq :: DexFile -> DexFile -> T.Assertion
deepEq df1 df2 = do
  T.assertEqual "idSrc" (dexIdSrc df1) (dexIdSrc df2)
  T.assertEqual "Types" (S.fromList (dexTypes df1)) (S.fromList (dexTypes df2))
  T.assertEqual "Constants" (S.fromList (dexConstants df1)) (S.fromList (dexConstants df2))
  T.assertEqual "NumClasses" (length (dexClasses df1)) (length (dexClasses df2))
  checkBoth dexClasses className checkClass df1 df2

checkClass :: Class -> Class -> T.Assertion
checkClass ex o = do
  T.assertEqual "classId" (classId ex) (classId o)
  T.assertEqual "classType" (classType ex) (classType o)
  T.assertEqual "className" (className ex) (className o)
  T.assertEqual "classSourceName" (classSourceName ex) (classSourceName o)
  T.assertEqual "classAccessFlags" (classAccessFlags ex) (classAccessFlags o)
  T.assertEqual "classParent" (classParent ex) (classParent o)
  T.assertEqual "classParentReference" (classParentReference ex) (classParentReference o)
  T.assertEqual "classInterfaces" (classInterfaces ex) (classInterfaces o)
  checkBoth classInstanceFields (fieldName . snd) checkField ex o
  checkBoth classStaticFields (fieldName . snd) checkField ex o
  checkBoth classDirectMethods methodKey checkMethod ex o
  checkBoth classVirtualMethods methodKey checkMethod ex o

methodKey :: Method -> (BS.ByteString, Type, [Type])
methodKey m = (methodName m, methodReturnType m, map parameterType (methodParameters m))

checkMethod :: Method -> Method -> T.Assertion
checkMethod ex o = do
  T.assertEqual "methodId" (methodId ex) (methodId o)
  T.assertEqual "methodName" (methodName ex) (methodName o)
  T.assertEqual "methodReturnType" (methodReturnType ex) (methodReturnType o)
  T.assertEqual "methodAccessFlags" (methodAccessFlags ex) (methodAccessFlags o)
  checkBoth methodParameters parameterIndex checkParameter ex o
  checkBoth methodParameters parameterId checkParameter ex o
  -- FIXME body
  T.assertEqual "methodClass" (methodClass ex) (methodClass o)

checkParameter :: Parameter -> Parameter -> T.Assertion
checkParameter ex o = do
  T.assertEqual "parameterId" (parameterId ex) (parameterId o)
  T.assertEqual "parameterType" (parameterType ex) (parameterType o)
  T.assertEqual "parameterName" (parameterName ex) (parameterName o)
  T.assertEqual "parameterIndex" (parameterIndex ex) (parameterIndex o)
  T.assertEqual "parameterMethod" (parameterMethod ex) (parameterMethod o)

checkField :: (AccessFlags, Field) -> (AccessFlags, Field) -> T.Assertion
checkField (ef, ex) (otf, o) = do
  T.assertEqual "field flagS" ef otf
  T.assertEqual "fieldId" (fieldId ex) (fieldId o)
  T.assertEqual "fieldName" (fieldName ex) (fieldName o)
  T.assertEqual "fieldType" (fieldType ex) (fieldType o)
  T.assertEqual "fieldClass" (fieldClass ex) (fieldClass o)

checkBoth :: (Show b, Ord c)
             => (a -> [b]) -- ^ Field to access from a
             -> (b -> c) -- ^ What to index elements by
             -> (b -> b -> T.Assertion) -- ^ Compare two elements (whose keys are equal)
             -> a -- ^ Expected
             -> a -- ^ Other
             -> T.Assertion
checkBoth accessFunc keyFunc assertion expected other = do
  let m1 = foldr addByKey M.empty (accessFunc expected)
      m2 = foldr addByKey M.empty (accessFunc other)
  mapM_ (expectedPresent m2) (accessFunc expected)
  mapM_ (otherPresent m1) (accessFunc other)
  where
    addByKey elt = M.insert (keyFunc elt) elt
    expectedPresent others ex = do
      case M.lookup (keyFunc ex) others of
        Nothing -> T.assertFailure ("Expected value " ++ show ex ++ " not found")
        Just o -> assertion ex o
    otherPresent expecteds o = do
      case M.lookup (keyFunc o) expecteds of
        Nothing -> T.assertFailure ("Found an extra value " ++ show o)
        Just ex -> assertion ex o
