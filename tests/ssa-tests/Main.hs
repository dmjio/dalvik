{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | End-to-end tests of the SSA transformation code.
--
-- The label tests in the other test suite are useful but don't cover
-- very much code.  These should hit nearly everything.
module Main ( main ) where

import Control.Failure
import Control.Monad ( liftM )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import Data.Set ( Set )
import qualified Data.Set as S

import System.FilePath
import System.FilePath.Glob ( namesMatching )

import Test.Framework as T
import Test.Framework.Providers.HUnit as T
import qualified Test.HUnit as T

import qualified Dalvik.Apk as Apk
import Dalvik.SSA

data TestDescriptor =
  SimplePhi { numBlocks :: Int
            , phis :: [(Int, Set String)]
              -- ^ Descriptions of phi nodes (incoming values) mapped to
              -- the block in which they are defined
            , blockPhis :: [(Int, Int)]
              -- ^ The pair is (phi node number, block number)
            , assignedPhis :: [(Int, String)]
              -- ^ A (phi node descriptor, global assigned to)
            }
  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = do
  expectedResults <- namesMatching "tests/testfiles/ssa-tests/*.expected"
  let tg = T.testGroup "SSA Tests" $ map makeTest expectedResults
  T.defaultMain [tg]

makeTest :: FilePath -> Test
makeTest expectedFile =
  T.testCase testName $ do
    eDF <- Apk.loadDexFromAnyIO input
    case eDF of
      Left err -> T.assertFailure err
      Right df -> do
        ssa <- toSSA Nothing [df]
        expectedValue <- liftM read (readFile expectedFile)
        case expectedValue of
          SimplePhi { numBlocks = nbs
                    , phis = phiDescriptors
                    , blockPhis = bphis
                    , assignedPhis = aphis
                    } -> do
            klass <- findClassByName (BS.pack testName) ssa
            Method { methodBody = Just blocks } <- findAnyMethodByName (BS.pack "entry") klass
            T.assertEqual "Wrong block count" nbs (length blocks)
            F.forM_ phiDescriptors $ \(phiNumber, incVals) -> do
              let Just bnum = lookup phiNumber bphis
              T.assertBool "Block out of bounds" (bnum < length blocks)
              let (phiInsts, _) = basicBlockSplitPhis (blocks !! bnum)
              T.assertBool ("Expected phi not found " ++ show phiInsts) (any (hasIncomingVals incVals) phiInsts)
              -- This one isn't irrefutable because there might not be
              -- an assignment for a given phi node.
              case lookup phiNumber aphis of
                Nothing -> return ()
                Just globalName ->
                  let p = any (assignsPhi incVals globalName) blocks
                  in T.assertBool "Missing phi assignment" p
  where
    input = dropExtension expectedFile
    testName = takeBaseName input

hasIncomingVals :: Set String -> Instruction -> Bool
hasIncomingVals incVals phiInst =
  case phiInst of
    Phi { phiValues = (map snd -> ivs) } ->
      incVals == S.fromList (map testFormat ivs)
    _ -> False

assignsPhi :: Set String -> String -> BasicBlock -> Bool
assignsPhi incVals gname = any assigns . basicBlockInstructions
  where
    assigns i =
      case i of
        StaticPut { staticOpField = fld
                  , staticOpPutValue = InstructionV p@(Phi {})
                  } ->
          fieldName fld == BS.pack gname && hasIncomingVals incVals p
        _ -> False

findAnyMethodByName :: (Failure LookupError f) => BS.ByteString -> Class -> f Method
findAnyMethodByName mname klass = maybe err return $ do
  F.find ((==mname) . methodName) ms
  where
    err = failure $ NoMethodFound mname "" (className klass)
    ms = classDirectMethods klass ++ classVirtualMethods klass

testFormat :: Value -> String
testFormat v =
  case v of
    ConstantV (ConstantString _ s) -> BS.unpack s
    _ -> show v
