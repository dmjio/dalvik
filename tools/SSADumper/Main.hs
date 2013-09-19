module Main ( main ) where

import qualified Dalvik.Types as DT
import Dalvik.SSA ( labelMethod )
import Dalvik.SSA.Labeling

import Tests.Dalvik.DexLoaders ( readAsDex, getEncodedMethod )

import System.Environment ( getArgs )
import Text.Printf (printf)

main :: IO ()
main = do
  [fileName, klass, method, sig] <- getArgs
  Right dexFile <- readAsDex fileName
  case getEncodedMethod dexFile klass method sig of
    Nothing -> error ("Could not find method: " ++ toStr klass method sig)
    Just m ->
      case labelMethod dexFile m of
        Left e -> error ("Could not label method: " ++ toStr klass method sig ++ " " ++ DT.decodeErrorAsString e)
        Right lbls -> putStrLn (prettyLabeling lbls)

toStr :: String -> String -> String -> String
toStr []      m sig = printf "%s%s" m sig
toStr c@(_:_) m sig | last c /= ';' = printf "%s%s%s" c m sig
                    | otherwise     = let
                      pkgName = (init c) ++ "."
                      in printf "%s%s%s" pkgName m sig

