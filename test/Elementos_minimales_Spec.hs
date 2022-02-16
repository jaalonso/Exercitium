module Elementos_minimales_Spec (main, spec) where

import Elementos_minimales
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "e1" $
      minimales [[1,3],[2,3,1],[3,2,5]]        `shouldBe`  [[2,3,1],[3,2,5]]
    it "e2" $
      minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  `shouldBe`  [[2,3,1],[3,2,5]]
    modifyMaxSize (const 7) $
      it "e3" $
      property prop_minimales
