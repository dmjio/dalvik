module Main ( main ) where

import Control.Monad ( liftM )
import qualified Data.ByteString as BS
import Data.Either ( rights )
import Data.String ( fromString )
import Options.Applicative
import Text.Printf ( printf )

import Dalvik.Apk
import qualified Dalvik.Types as DT
import Dalvik.SSA.Internal.Labeling
import Dalvik.SSA

data Options = Options { optCommand :: Command }

data Command =
  LabelCommand { labelFilename :: FilePath
               , labelClassName :: String
               , labelMethodName :: String
               , labelTypeSignature :: String
               }
  | PrettyCommand { prettyFilename :: [FilePath]
                  , prettyClassName :: Maybe String
                  , prettyMethodName :: Maybe String
                  , prettyTypeSignature :: Maybe String
                  , prettyStubs :: Maybe FilePath
                  , prettyStubsPrefix :: Maybe String
                  , prettyOutput :: Maybe FilePath
                  , prettyBase :: Maybe FilePath
                    -- ^ A base dex file to build on
                  }
  | LoadCommand { loadFilename :: FilePath }
  | DumpCommand { dumpFilename :: FilePath }

commandStubs :: Command -> IO (Maybe Stubs)
commandStubs PrettyCommand { prettyStubs = Just stubFile
                           , prettyStubsPrefix = Just prefix
                           } = do
  edx <- loadDexFromAnyIO stubFile
  case edx of
    Left err -> putStrLn ("Stubs " ++ err) >> return Nothing
    Right dx -> return $ Just (stubs (fromString prefix) dx)
commandStubs _ = return Nothing

optionParser :: Parser Options
optionParser = Options <$>
                subparser (  command "label" (info labelOptions (progDesc "Dump SSA value labels"))
                             <> command "pretty" (info prettyOptions (progDesc "Pretty print the SSA IR"))
                             <> command "load" (info loadOptions (progDesc "Load and print serialized SSA IR"))
                             <> command "dump" (info dumpOptions (progDesc "Dump a serialized SSA IR file"))
                          )
  where
    loadOptions = LoadCommand <$> strOption ( long "filename"
                                              <> short 'f'
                                              <> metavar "FILE"
                                              <> help "The serialized dalvik to load" )
    dumpOptions = DumpCommand <$> strOption ( long "filename"
                                              <> short 'f'
                                              <> metavar "FILE"
                                              <> help "The serialized dalvik to load" )
    prettyOptions = PrettyCommand <$>
      some (strOption ( long "filename"
                  <> short 'f'
                  <> metavar "FILE"
                  <> help "The java file to parse"))
      <*> optional (strOption ( long "class"
                                <> short 'c'
                                <> metavar "CLASS"
                                <> help "The name of the class containing the target method"))
      <*> optional (strOption ( long "method"
                                <> short 'm'
                                <> metavar "METHOD"
                                <> help "The name of the target method"))
      <*> optional (strOption ( long "signature"
                                <> short 's'
                                <> metavar "SIGNATURE"
                                <> help "The signature of the target method"))
      <*> optional (strOption ( long "stubs"
                               <> metavar "FILE"
                               <> help "A file containing stub implementations"))
      <*> optional (strOption ( long "stub-prefix"
                                <> metavar "PREFIX"
                                <> help "A prefix to strip from package names in the stub bundle"))
      <*> optional (strOption ( long "output"
                                <> short 'o'
                                <> metavar "FILE"
                                <> help "A file to save the SSA IR to" ))
      <*> optional (strOption ( long "base"
                                <> short 'b'
                                <> metavar "FILE"
                                <> help "A base Dex file to build on (previously serialized)"))


    labelOptions = LabelCommand <$>
        strOption ( long "filename"
                    <> short 'f'
                    <> metavar "FILE"
                    <> help "The java file to parse")
        <*> strOption ( long "class"
                        <> short 'c'
                        <> metavar "CLASS"
                        <> help "Class containing the target method")
        <*> strOption ( long "method"
                        <> short 'm'
                        <> metavar "METHOD"
                        <> help "Name of the target method")
        <*> strOption ( long "signature"
                        <> short 's'
                        <> metavar "SIGNATURE"
                        <> help "Type signature of the target method")


main :: IO ()
main = execParser opts >>= realMain
  where
    opts = info (helper <*> optionParser) ( fullDesc <> progDesc "Simple viewer for the dalvik SSA IR" <> header "SSADumper" )


realMain :: Options -> IO ()
realMain Options { optCommand =
                      LabelCommand { labelFilename = fileName
                                   , labelClassName = klass
                                   , labelMethodName = method
                                   , labelTypeSignature = sig
                                   } } = do
  Right dexFile <- loadDexFromAnyIO fileName
  case DT.getEncodedMethod dexFile klass method sig of
    Nothing -> error ("Could not find method: " ++ toStr klass method sig)
    Just m -> do
      lbls <- labelMethod dexFile m
      putStrLn (prettyLabeling lbls)
  -- case DT.getEncodedMethod dexFile klass method sig of
  --   Nothing -> error ("Could not find method: " ++ toStr klass method sig)
  --   Just m ->
  --     case labelMethod dexFile m of
  --       Left e -> error ("Could not label method: " ++ toStr klass method sig ++ " " ++ DT.decodeErrorAsString e)
  --       Right lbls -> putStrLn (prettyLabeling lbls)
realMain Options { optCommand = LoadCommand { loadFilename = fileName } } = do
  bs <- BS.readFile fileName
  case deserializeDex bs of
    Left err -> error err
    Right ssa -> print (length (dexClasses ssa))
realMain Options { optCommand = DumpCommand { dumpFilename = fileName } } = do
  bs <- BS.readFile fileName
  case deserializeDex bs of
    Left err -> error err
    Right ssa -> print ssa
realMain Options { optCommand = pc@PrettyCommand { prettyFilename = fileName
                                                 , prettyClassName = Nothing
                                                 , prettyOutput = output
                                                 , prettyBase = baseFile
                                                 } } = do
  dexFiles <- liftM rights $ mapM loadDexFromAnyIO fileName
  st <- commandStubs pc
  base <- loadBase baseFile
  ssaDex <- toSSA st base dexFiles
  print ssaDex
  case output of
    Nothing -> return ()
    Just outFile -> BS.writeFile outFile $ serializeDex ssaDex
realMain Options { optCommand = pc@PrettyCommand { prettyFilename = fileName
                                                 , prettyClassName = Just cname
                                                 , prettyMethodName = Nothing
                                                 , prettyTypeSignature = Nothing
                                                 } } = do
  dexFiles <- liftM rights $ mapM loadDexFromAnyIO fileName
  st <- commandStubs pc
  ssaDex <- toSSA st Nothing dexFiles
  klass <- findClassByName (fromString cname) ssaDex
  print klass
realMain Options { optCommand = pc@PrettyCommand { prettyFilename = fileName
                                                 , prettyClassName = Just cname
                                                 , prettyMethodName = Just mname
                                                 , prettyTypeSignature = Just sig
                                                 } } = do
  dexFiles <- liftM rights $ mapM loadDexFromAnyIO fileName
  st <- commandStubs pc
  ssaDex <- toSSA st Nothing dexFiles
  klass <- findClassByName (fromString cname) ssaDex
  method <- findMethodByNameString (fromString mname) sig klass
  print method
realMain _ = error "You must specify a method and a signature"


loadBase :: Maybe FilePath -> IO (Maybe DexFile)
loadBase Nothing = return Nothing
loadBase (Just baseFile) = do
  bs <- BS.readFile baseFile
  case deserializeDex bs of
    Right df -> return (Just df)
    Left err -> error err

toStr :: String -> String -> String -> String
toStr []      m sig = printf "%s%s" m sig
toStr c@(_:_) m sig | last c /= ';' = printf "%s%s%s" c m sig
                    | otherwise     = let
                      pkgName = (init c) ++ "."
                      in printf "%s%s%s" pkgName m sig

