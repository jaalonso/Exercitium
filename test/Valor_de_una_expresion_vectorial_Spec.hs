module Valor_de_una_expresion_vectorial_Spec (main, spec) where

import Valor_de_una_expresion_vectorial
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (ExpV -> (Int,Int)) -> Spec
specG valorEV = do
  it "e1" $
    valorEV (Vec 1 2)                                  `shouldBe`  (1,2)
  it "e2" $
    valorEV (Sum (Vec 1 2) (Vec 3 4))                  `shouldBe`  (4,6)
  it "e3" $
    valorEV (Mul 2 (Vec 3 4))                          `shouldBe`  (6,8)
  it "e4" $
    valorEV (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         `shouldBe`  (8,12)
  it "e5" $
    valorEV (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  `shouldBe`  (8,12)

spec :: Spec
spec = do
  describe "def. 1" $ specG valorEV1
  describe "def. 2" $ specG valorEV2
