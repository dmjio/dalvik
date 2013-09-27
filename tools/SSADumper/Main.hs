module Main ( main ) where

import Options.Applicative
import Text.Printf ( printf )

import qualified Dalvik.Types as DT
import Dalvik.SSA.Internal.Labeling
import Dalvik.SSA
import Tests.Dalvik.DexLoaders ( readAsDex, getEncodedMethod )

data Options = Options { optCommand :: Command }

data Command =
  LabelCommand { labelFilename :: FilePath
               , labelClassName :: String
               , labelMethodName :: String
               , labelTypeSignature :: String
               }
  | PrettyCommand { prettyFilename :: FilePath
                  , prettyClassName :: Maybe String
                  , prettyMethodName :: Maybe String
                  , prettyTypeSignature :: Maybe String
                  }

optionParser :: Parser Options
optionParser = Options <$>
                subparser (  command "label" (info labelOptions (progDesc "Dump SSA value labels"))
                             <> command "pretty" (info prettyOptions (progDesc "Pretty print the SSA IR"))
                          )
  where
    prettyOptions = PrettyCommand <$>
      strOption ( long "filename"
                  <> short 'f'
                  <> metavar "FILE"
                  <> help "The java file to parse")
      <*> optional (strOption ( long "class"
                                <> short 'c'
                                <> metavar "CLASS"
                                <> help "The name of the class containing the target method"))
      <*> optional (strOption ( long "method"
                                <> short ','
                                <> metavar "METHOD"
                                <> help "The name of the target method"))
      <*> optional (strOption ( long "signature"
                                <> short 's'
                                <> metavar "SIGNATURE"
                                <> help "The signature of the target method"))

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
  Right dexFile <- readAsDex fileName
  case getEncodedMethod dexFile klass method sig of
    Nothing -> error ("Could not find method: " ++ toStr klass method sig)
    Just m ->
      case labelMethod dexFile m of
        Left e -> error ("Could not label method: " ++ toStr klass method sig ++ " " ++ DT.decodeErrorAsString e)
        Right lbls -> putStrLn (prettyLabeling lbls)
realMain Options { optCommand = PrettyCommand { prettyFilename = fileName
                                              , prettyClassName = Nothing
                                              } } = do
  Right dexFile <- readAsDex fileName
  ssaDex <- toSSA dexFile
  print ssaDex

toStr :: String -> String -> String -> String
toStr []      m sig = printf "%s%s" m sig
toStr c@(_:_) m sig | last c /= ';' = printf "%s%s%s" c m sig
                    | otherwise     = let
                      pkgName = (init c) ++ "."
                      in printf "%s%s%s" pkgName m sig

