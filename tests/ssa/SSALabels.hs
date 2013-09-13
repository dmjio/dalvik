module Main ( main ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Word ( Word16 )
import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import Test.HUnit as T

import Dalvik.Instruction as I
import Dalvik.SSA.Labelling

argMap :: [(String, Word16)]
argMap = [ ("arg1", 99)
         , ("arg2", 100)
         , ("arg3", 101)
         ]

showProgram :: [Instruction] -> IO ()
showProgram = putStrLn . (++ "\n\n") . prettyLabelling . labelInstructions argMap

main :: IO ()
main = do
  mapM_ showProgram [p1, p2, p3, p4]
  T.defaultMain $ [
    T.testGroup "basic-tests" [
       T.testCase "addition-sequence" (checkReturnValue p1 (SimpleLabel 5))
       , T.testCase "move-sequence" (checkReturnValue p2 (SimpleLabel 3))
       , T.testCase "return-moved-argument" (checkReturnValue p3 (ArgumentLabel "arg1" 0))
       , T.testCase "trivial-branch" (checkReturnValue p4 (PhiLabel 2 5))
       ]
    ]

isReturn :: Instruction -> Bool
isReturn i =
  case i of
    Return _ _ -> True
    _ -> False

checkReturnValue :: [Instruction] -> Label -> Assertion
checkReturnValue p expected = fromMaybe (T.assertFailure "No return instruction found") $ do
  (_, m) <- L.find (isReturn . fst) instMaps
  case M.toList m of
    [(_, label) ] -> return $ do
      T.assertBool "Non-unique value label" (generatedLabelsAreUnique l)
      T.assertEqual "Unexpected label mismatch" expected label
    _ -> return $ T.assertFailure "More than one (or no) register mapped for return"
  where
    l = labelInstructions argMap p
    instMaps = M.toList (labellingReadRegs l)

p1 :: [Instruction]
p1 = [ IBinop Add False 2 99 100
     , IBinop Add False 3 2 99
     , IBinop Add False 3 3 2
     , Return MNormal (R8 3)
     ]


p2 :: [Instruction]
p2 = [ IBinop Add False 2 99 100
     , Move MNormal (R8 3) (R8 2)
     , Move MNormal (R8 4) (R8 3)
     , Move MNormal (R8 5) (R8 4)
     , Return MNormal (R8 5)
     ]

p3 :: [Instruction]
p3 = [ Move MNormal (R8 0) (R8 99)
     , Move MNormal (R8 1) (R8 0)
     , Move MNormal (R8 2) (R8 1)
     , Return MNormal (R8 2)
     ]

-- | A simple branch:
--
-- > int f(int x) {
-- >   if(x > 0)
-- >     x = x * 2;
-- >   return x;
p4 :: [Instruction]
p4 = [ Move MNormal (R8 0) (R8 99)
     , IfZero Le 0 3
     , LoadConst (R8 1) (Const4 2)
     , IBinop Add False 0 0 1
     , Return MNormal (R8 0)
     ]
