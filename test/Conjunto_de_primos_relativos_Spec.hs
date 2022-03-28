module Conjunto_de_primos_relativos_Spec (main, spec) where

import Conjunto_de_primos_relativos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> Bool) -> Spec
specG primosRelativos = do
  it "e1" $
    primosRelativos [6,35]         `shouldBe`  True
  it "e2" $
    primosRelativos [6,27]         `shouldBe`  False
  it "e3" $
    primosRelativos [2,3,4]        `shouldBe`  False
  it "e4" $
    primosRelativos [6,35,11]      `shouldBe`  True
  it "e5" $
    primosRelativos [6,35,11,221]  `shouldBe`  True
  it "e6" $
    primosRelativos [6,35,11,231]  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG primosRelativos1
  describe "def. 2" $ specG primosRelativos2
  describe "def. 3" $ specG primosRelativos3
  describe "def. 4" $ specG primosRelativos4
  describe "def. 5" $ specG primosRelativos5
