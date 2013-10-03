-- | Helpers for conveniently loading dalvik source.
module Dalvik.Apk (
  loadDexFromApkIO,
  loadDexFromAnyIO,
  DexReader,
  memoIO
  ) where

import Codec.Archive.Zip
import Control.Concurrent ( newMVar, modifyMVar )
import qualified Data.ByteString as BS
import Data.Conduit.List ( consume )
import qualified Data.List as L
import qualified Data.Map as M
import System.Cmd ( rawSystem )
import System.Directory ( getDirectoryContents )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), takeFileName, takeExtension )
import System.IO
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( runProcess, waitForProcess )

import Dalvik.Parser
import Dalvik.Types as DT

-- | Load the first .dex file found in an Apk
loadDexFromApkIO :: FilePath -> IO (Either String DexFile)
loadDexFromApkIO f = do
  chunks <- withArchive f (sourceEntry "classes.dex" consume)
  -- TODO: this is silly. Should we tweak the parser to work with
  -- lazy ByteStrings?
  return . loadDex . BS.concat $ chunks

-- | Load the first .dex file found in an Apk, a raw .dex file, or a
-- Java source file.
loadDexFromAnyIO :: FilePath -> IO (Either String DexFile)
loadDexFromAnyIO f
  | takeExtension f == ".java" = readSourceAsDex f
  | otherwise  = do
    h <- openFile f ReadMode
    c <- hGetChar h
    hClose h
    case c of
      'P' -> loadDexFromApkIO f
      'd' -> loadDexIO f
      _ -> return (Left "Invalid file format")


type DexReader = FilePath -> IO (Either String DT.DexFile)

-- | Read a file (.java, .class, or .jar) as a dex file by performing
-- the required pre-processing to generate a dex file and load it with
-- the dex parser.
readSourceAsDex :: FilePath -> IO (Either String DT.DexFile)
readSourceAsDex fname = withSystemTempDirectory "fuse.tmp" $
  \dirName -> do
    dexFilePath <- runDX fname dirName
    loadDexIO dexFilePath

-- | Run the dx tool on the incoming file, outputting to the directory
-- specified, and returning the filepath of the resulting dex
-- bytecode.
runDX :: FilePath -> FilePath -> IO FilePath
runDX input targetDir = do
  ec1 <- rawSystem "javac" ["-d", targetDir, input]
  case ec1 of
    ExitFailure err -> error ("Error running `javac` on " ++ input ++ ": "++ show err)
    ExitSuccess -> do
      args <- dxArgs
      ec2 <- runWithWD "dx" args targetDir
      case ec2 of
        ExitFailure err -> error ("Error running `dx` " ++ show err)
        ExitSuccess -> return outFilePath
  where
    inFileName = takeFileName input

    outFile = inFileName ++ ".dex"
    outFilePath = targetDir </> outFile

    classFiles = (filter (".class" `L.isSuffixOf`))
                 `fmap` (getDirectoryContents targetDir)

    dxArgs = do
      classes <- classFiles
      return (["--dex", "--output="++outFile] ++ classes)

runWithWD :: FilePath -> [String] -> FilePath -> IO ExitCode
runWithWD cmd args wDir = do
  hdl <- runProcess cmd args (Just wDir)
          Nothing Nothing Nothing Nothing
  waitForProcess hdl

memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO f = do
  ref <- newMVar M.empty
  return (memoIO' ref)
  where
    memoIO' ref a = modifyMVar ref $ \m -> do
      case M.lookup a m of
        Just v -> return (m, v)
        Nothing -> do
          v <- f a
          return (M.insert a v m, v)

