module Main ( main ) where

import Dalvik.Instruction as I
import Dalvik.SSA.Labelling

main :: IO ()
main = do
  putStrLn $ prettyLabelling (labelInstructions p1)
  putStrLn $ prettyLabelling (labelInstructions p2)

p1 :: [Instruction]
p1 = [ IBinop Add False 2 1 0
     , IBinop Add False 3 2 1
     , IBinop Add False 3 3 2
     , Return MNormal (R8 3)
     ]


p2 :: [Instruction]
p2 = [ IBinop Add False 2 1 0
     , Move MNormal (R8 3) (R8 2)
     , Move MNormal (R8 4) (R8 3)
     , Move MNormal (R8 5) (R8 4)
     , Return MNormal (R8 5)
     ]
