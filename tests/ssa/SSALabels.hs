{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Test.Framework as T

import qualified Tests.Dalvik.SSA as TSSA
import qualified Tests.Dalvik.Labels as LSSA
import qualified Tests.Dalvik.NameDecoding as ND

main :: IO ()
main = do
  T.defaultMain $ [
    ND.tests
    , TSSA.tests
    , LSSA.tests
    ]

