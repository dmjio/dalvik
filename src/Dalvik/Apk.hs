-- | Helpers for conveniently loading dalvik source.
module Dalvik.Apk (
  loadDexFromApkIO,
  loadDexFromAnyIO,
  DexReader,
  runDX,
  memoIO
  ) where

import qualified Codec.Archive.Zip as Z
import Control.Applicative
import Control.Concurrent ( newMVar, modifyMVar )
import Control.Exception ( handle, ErrorCall )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import System.Directory ( getDirectoryContents )
import System.Environment ( getEnvironment )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), (<.>), takeFileName, takeExtension )
import System.IO
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( runProcess, waitForProcess, readProcessWithExitCode )

import Dalvik.Parser
import Dalvik.Types as DT

-- | Load the first .dex file found in an Apk
loadDexFromApkIO :: FilePath -> IO (Either String DexFile)
loadDexFromApkIO f = handle handler $ do
  a <- Z.toArchive <$> LBS.readFile f
  case Z.findEntryByPath "classes.dex" a of
    Nothing -> return $ Left ("Could not find classes.dex in " ++ f)
    -- Should we have the parser work over lazy bytestrings instead?
    Just e -> return $ loadDex $ LBS.toStrict (Z.fromEntry e)

  where handler :: ErrorCall -> IO (Either String DexFile)
        handler err = return (Left ("Could not find classes.dex in apk: " ++ show err))

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
    let outFile = takeFileName fname <.> "dex"
    dexFilePath <- runDX [fname] outFile dirName
    loadDexIO dexFilePath

androidJarPath :: IO [String]
androidJarPath = do
  env <- getEnvironment
  case lookup "DALVIK_EXTRA_JAR" env of
    Nothing -> return []
    Just path -> return ["-classpath", path]

-- | Run the dx tool on the incoming file, outputting to the directory
-- specified, and returning the filepath of the resulting dex
-- bytecode.
runDX :: [FilePath] -> FilePath -> FilePath -> IO FilePath
runDX inputs outFile targetDir = do
  cp <- androidJarPath
  (ec1, _, _) <- readProcessWithExitCode "javac" (cp ++ "-d" : targetDir : inputs) ""
  case ec1 of
    ExitFailure err -> error ("Error running `javac` on " ++ show inputs ++ ": "++ show err)
    ExitSuccess -> do
      args <- dxArgs
      ec2 <- runWithWD "dx" args targetDir
      case ec2 of
        ExitFailure err -> error ("Error running `dx` " ++ show err)
        ExitSuccess -> return outFilePath
  where
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

