module Numero_de_particiones_en_k_subconjuntos_Spec (main, spec) where

import Numero_de_particiones_en_k_subconjuntos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Int -> Int) -> Spec
specG numeroParticiones = do
  it "e1" $
    numeroParticiones 3 2 `shouldBe`  3
  it "e2" $
    numeroParticiones 3 3 `shouldBe`  1
  it "e3" $
    numeroParticiones 4 3 `shouldBe`  6
  it "e4" $
    numeroParticiones 4 1 `shouldBe`  1
  it "e5" $
    numeroParticiones 4 4 `shouldBe`  1

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroParticiones1
  describe "def. 2" $ specG numeroParticiones2
  describe "def. 3" $ specG numeroParticiones3
