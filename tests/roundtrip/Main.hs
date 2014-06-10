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
main = T.defaultMain [t1, t2]
  where
    t1 = T.testCase "Roundtrip Test 1" $ do
      Right core <- Apk.loadDexFromAnyIO "tests/roundtrip/core.dex"
      Right frame <- Apk.loadDexFromAnyIO "tests/roundtrip/framework.dex"
      Right app <- Apk.loadDexFromAnyIO "tests/roundtrip/CreateChooser.apk"
      ssa0 <- toSSA Nothing Nothing [core, frame]
      let Right ssa1 = deserializeDex $ serializeDex ssa0
      expected <- toSSA Nothing Nothing [core, frame, app]
      actual <- toSSA Nothing (Just ssa1) [app]
      deepEq expected actual
    t2 = T.testCase "Roundtrip Test 2" $ do
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
  checkBoth classInstanceFields (fieldId . snd) checkField ex o
  checkBoth classStaticFields (fieldId . snd) checkField ex o
  checkBoth classInstanceFields (fieldName . snd) checkField ex o
  checkBoth classStaticFields (fieldName . snd) checkField ex o
  checkBoth classDirectMethods methodKey checkMethod ex o
  checkBoth classVirtualMethods methodKey checkMethod ex o
  checkBoth classDirectMethods methodId checkMethod ex o
  checkBoth classVirtualMethods methodId checkMethod ex o

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
  T.assertEqual "methodClass" (methodClass ex) (methodClass o)
  checkBoth fromBody basicBlockId checkBlock (methodBody ex) (methodBody o)

fromBody :: Maybe [a] -> [a]
fromBody Nothing = []
fromBody (Just b) = b

checkBlock :: BasicBlock -> BasicBlock -> T.Assertion
checkBlock ex o = do
  T.assertEqual "basicBlockId" (basicBlockId ex) (basicBlockId o)
  T.assertEqual "basicBlockNumber" (basicBlockNumber ex) (basicBlockNumber o)
  T.assertEqual "basicBlockPhiCount" (basicBlockPhiCount ex) (basicBlockPhiCount o)
  T.assertEqual "basicBlockSuccessors" (basicBlockSuccessors ex) (basicBlockSuccessors o)
  T.assertEqual "basicBlockPredecessors" (basicBlockPredecessors ex) (basicBlockPredecessors o)
  T.assertEqual "basicBlockMethod" (basicBlockMethod ex) (basicBlockMethod o)
  checkBoth basicBlockInstructions instructionId checkInstruction ex o

checkInstruction :: Instruction -> Instruction -> T.Assertion
checkInstruction ex o = do
  T.assertEqual "instructionId" (instructionId ex) (instructionId o)
  T.assertEqual "instructionType" (instructionType ex) (instructionType o)
  T.assertEqual "instructionBasicBlock" (instructionBasicBlock ex) (instructionBasicBlock o)
  case (ex, o) of
    (Return { returnValue = rex }, Return { returnValue = ro }) -> do
      T.assertEqual "returnValue" rex ro
    (MoveException {}, MoveException {}) -> return ()
    (MonitorEnter { monitorReference = rex }, MonitorEnter { monitorReference = ro }) -> do
      T.assertEqual "monitorReference" rex ro
    (MonitorExit { monitorReference = rex }, MonitorExit { monitorReference = ro }) -> do
      T.assertEqual "monitorReference" rex ro
    (CheckCast { castReference = rex, castType = tex },
     CheckCast { castReference = ro, castType = to }) -> do
      T.assertEqual "castReference" rex ro
      T.assertEqual "castType" tex to
    (InstanceOf { instanceOfReference = rex, instanceOfType = tex },
     InstanceOf { instanceOfReference = ro, instanceOfType = to }) -> do
      T.assertEqual "instanceOfReference" rex ro
      T.assertEqual "instanceOfType" tex to
    (ArrayLength { arrayReference = rex }, ArrayLength { arrayReference = ro }) -> do
      T.assertEqual "arrayReference" rex ro
    (NewInstance {}, NewInstance {}) -> return ()
    (NewArray { newArrayLength = nlex, newArrayContents = cex },
     NewArray { newArrayLength = nlo, newArrayContents = co }) -> do
      T.assertEqual "newArrayLength" nlex nlo
      T.assertEqual "newArrayContents" cex co
    (FillArray { fillArrayReference = rex, fillArrayContents = cex },
     FillArray { fillArrayReference = ro, fillArrayContents = co }) -> do
      T.assertEqual "fillArrayReference" rex ro
      T.assertEqual "fillArrayContents" cex co
    (Throw { throwReference = rex }, Throw { throwReference = ro }) -> do
      T.assertEqual "throwReference" rex ro
    (ConditionalBranch { branchOperand1 = op1ex, branchOperand2 = op2ex, branchTestType = ttex, branchTarget = btex, branchFallthrough = bfex },
     ConditionalBranch { branchOperand1 = op1o, branchOperand2 = op2o, branchTestType = tto, branchTarget = bto, branchFallthrough = bfo }) -> do
      T.assertEqual "branchOperand1" op1ex op1o
      T.assertEqual "branchOperand2" op2ex op2o
      T.assertEqual "branchTestType" ttex tto
      T.assertEqual "branchTarget" btex bto
      T.assertEqual "branchFallthrough" bfex bfo
    (UnconditionalBranch { branchTarget = btex }, UnconditionalBranch { branchTarget = bto }) -> do
      T.assertEqual "branchTarget" btex bto
    (Switch { switchValue = svex, switchTargets = stex, switchFallthrough = sfex },
     Switch { switchValue = svo, switchTargets = sto, switchFallthrough = sfo }) -> do
      T.assertEqual "switchValue" svex svo
      T.assertEqual "switchTargets" stex sto
      T.assertEqual "switchFallthrough" sfex sfo
    (Compare { compareOperation = cex, compareOperand1 = op1ex, compareOperand2 = op2ex },
     Compare { compareOperation = co, compareOperand1 = op1o, compareOperand2 = op2o }) -> do
      T.assertEqual "compareOperation" cex co
      T.assertEqual "compareOperand1" op1ex op1o
      T.assertEqual "compareOperand2" op2ex op2o
    (UnaryOp { unaryOperand = opex, unaryOperation = oex },
     UnaryOp { unaryOperand = opo, unaryOperation = oo }) -> do
      T.assertEqual "unaryOperand" opex opo
      T.assertEqual "unaryOperation" oex oo
    (BinaryOp { binaryOperand1 = op1ex, binaryOperand2 = op2ex, binaryOperation = opex },
     BinaryOp { binaryOperand1 = op1o, binaryOperand2 = op2o, binaryOperation = opo }) -> do
      T.assertEqual "binaryOperand1" op1ex op1o
      T.assertEqual "binaryOperand2" op2ex op2o
      T.assertEqual "binaryOperation" opex opo
    (ArrayGet { arrayReference = arex, arrayIndex = aiex },
     ArrayGet { arrayReference = aro, arrayIndex = aio }) -> do
      T.assertEqual "arrayReference" arex aro
      T.assertEqual "arrayIndex" aiex aio
    (ArrayPut {arrayReference = arex, arrayIndex = aiex, arrayPutValue = pvex },
     ArrayPut {arrayReference = aro, arrayIndex = aio, arrayPutValue = pvo }) -> do
      T.assertEqual "arrayReference" arex aro
      T.assertEqual "arrayIndex" aiex aio
      T.assertEqual "arrayPutValue" pvex pvo
    (StaticGet { staticOpField = fex }, StaticGet { staticOpField = fo }) -> do
      T.assertEqual "staticOpField" fex fo
    (StaticPut { staticOpField = fex, staticOpPutValue = pex },
     StaticPut { staticOpField = fo, staticOpPutValue = po }) -> do
      T.assertEqual "staticOpField" fex fo
      T.assertEqual "staticOpPutValue" pex po
    (InstanceGet { instanceOpReference = rex, instanceOpField = fex },
     InstanceGet { instanceOpReference = ro, instanceOpField = fo }) -> do
      T.assertEqual "instanceOpReference" rex ro
      T.assertEqual "instanceOpField" fex fo
    (InstancePut { instanceOpReference = rex, instanceOpField = fex, instanceOpPutValue = pex },
     InstancePut { instanceOpReference = ro, instanceOpField = fo, instanceOpPutValue = po }) -> do
      T.assertEqual "instanceOpReference" rex ro
      T.assertEqual "instanceOpField" fex fo
      T.assertEqual "instanceOpPutValue" pex po
    (InvokeVirtual { invokeVirtualKind = kex, invokeVirtualMethod = mex, invokeVirtualArguments = aex },
     InvokeVirtual { invokeVirtualKind = ko, invokeVirtualMethod = mo, invokeVirtualArguments = ao }) -> do
      T.assertEqual "invokeVirutalKind" kex ko
      T.assertEqual "invokeVirtualMethod" mex mo
      T.assertEqual "invokeVirtualArguments" aex ao
    (InvokeDirect { invokeDirectKind = kex, invokeDirectMethod = mrefex, invokeDirectMethodDef = mex, invokeDirectArguments = aex },
     InvokeDirect { invokeDirectKind = ko, invokeDirectMethod = mrefo, invokeDirectMethodDef = mo, invokeDirectArguments = ao }) -> do
      T.assertEqual "invokeDirectKind" kex ko
      T.assertEqual "invokeDirecTMethod" mrefex mrefo
      T.assertEqual "invokeDirectMethodDef" mex mo
      T.assertEqual "invokeDirectArguments" aex ao
    (Phi { phiValues = pvex }, Phi { phiValues = pvo }) -> do
      T.assertEqual "phiValues" pvex pvo
    _ -> T.assertFailure ("Instruction mismatch: expected " ++ show ex ++ " got " ++ show o)

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
