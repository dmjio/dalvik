{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Test.Framework as T

import qualified Tests.Dalvik.SSA as TSSA
import qualified Tests.Dalvik.Labels as LSSA

main :: IO ()
main = do
  T.defaultMain $ [
     TSSA.tests
    , LSSA.tests
    ]

