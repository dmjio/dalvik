module Main ( main ) where

import Data.Word ( Word16 )

import Dalvik.Instruction as I
import Dalvik.SSA.Labelling

argMap :: [(String, Word16)]
argMap = [ ("arg1", 99)
         , ("arg2", 100)
         , ("arg3", 101)
         ]

main :: IO ()
main = do
  putStrLn $ prettyLabelling (labelInstructions argMap p1)
  putStrLn $ prettyLabelling (labelInstructions argMap p2)

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
