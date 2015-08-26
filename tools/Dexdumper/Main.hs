{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Foldable as F
import qualified Data.Map as Map
import System.Environment
import qualified System.IO as IO
import qualified Text.PrettyPrint.HughesPJ as PP

import Dalvik.Apk
import Dalvik.Printer
import Dalvik.Types

processFile :: FilePath -> IO ()
processFile f = do
  edex <- loadDexFromAnyIO f
  case edex of
    Left err -> do
      IO.hPutStrLn IO.stderr ("Error while processing file " ++ f)
      IO.hPutStrLn IO.stderr err
    Right dex -> do
      putStrLn ("Processing '" ++ f ++ "'...")
      LBS.putStrLn (LBS.pack (PP.render (prettyHeader f (dexHeader dex))))
      LBS.putStrLn ""
      F.forM_ (Map.toList (dexClasses dex)) $ \cls -> do
        LBS.putStrLn (LBS.pack (PP.render (prettyClassDef dex cls)))
        LBS.putStrLn ""

main :: IO ()
main = mapM_ processFile =<< getArgs
