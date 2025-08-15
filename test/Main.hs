module Main (main) where

import APITest qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "BgmTV Tests"
    [ APITest.tests
    ]
