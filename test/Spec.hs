module Main where

import Test.Hspec

import qualified Web.Bgmtv.TypesSpec

main :: IO ()
main = hspec $ do
  describe "Web.Bgmtv.Types" Web.Bgmtv.TypesSpec.spec
