module Tests.Dalvik.DexLoaders where

import Control.Monad ( guard )
import Data.ByteString ()
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )

import System.Cmd (rawSystem)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (runProcess, waitForProcess)

import Dalvik.Types as DT

import Dalvik.Parser (loadDexIO)

getEncodedMethod :: DexFile -> String -> String -> String -> Maybe EncodedMethod
getEncodedMethod dx className methodName typeSig = do
  let classes = M.elems $ dexClasses dx
  c <- L.find (maybe False (==fromString className) . getTypeName dx . classId) classes
  let ms = classDirectMethods c ++ classVirtualMethods c
  L.find checkMethod ms
  where
    checkMethod encMeth = fromMaybe False $ do
      m <- getMethod dx (methId encMeth)
      mname <- getStr dx (methNameId m)
      guard (mname == fromString methodName)
      p <- getProto dx (methProtoId m)
      shortDesc <- getStr dx (protoShortDesc p)
      return $ fromString typeSig == shortDesc

-- | Read a file (.java, .class, or .jar) as a dex file by performing
-- the required pre-processing to generate a dex file and load it with
-- the dex parser.
readAsDex :: FilePath -> IO (Either String DT.DexFile)
readAsDex fname = withSystemTempDirectory "fuse.tmp" $
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