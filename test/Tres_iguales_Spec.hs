module Tres_iguales_Spec (main, spec) where

import Tres_iguales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec  = do
  it "e1" $
    tresIguales 4 4 4  `shouldBe`  True
  it "e2" $
    tresIguales 4 3 4  `shouldBe`  False
