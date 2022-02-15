module Bandera_tricolor_Spec (main, spec) where

import Bandera_tricolor
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "e1" $
      banderaTricolor1 [M,R,A,A,R,R,A,M,M]  `shouldBe`  [R,R,R,A,A,A,M,M,M]
    it "e2" $
      banderaTricolor1 [M,R,A,R,R,A]        `shouldBe`  [R,R,R,A,A,M]
    it "e3" $
      property prop_banderaTricolor
