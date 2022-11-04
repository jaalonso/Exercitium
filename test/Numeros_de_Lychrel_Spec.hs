module Numeros_de_Lychrel_Spec (main, spec) where

import Numeros_de_Lychrel
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "esCapicua" $ do
    it "e1" $
      esCapicua 252  `shouldBe`  True
    it "e2" $
      esCapicua 253  `shouldBe`  False

  describe "inverso" $
    it "e1" $
      inverso 253  `shouldBe`  352

  describe "siguiente" $
    it "e1" $
      siguiente 253  `shouldBe`  605

  describe "busquedaDeCapicua" $
    it "e1" $
      busquedaDeCapicua 253  `shouldBe`  [253,605,1111]

  describe "capicuaFinal" $
    it "e1" $
      capicuaFinal 253  `shouldBe`  1111

  describe "orden" $
    it "e1" $
      orden 253  `shouldBe`  2

  describe "ordenMayor" $ do
    it "e1" $
      ordenMayor 1186060307891929990 2 `shouldBe` True
    it "e2" $
      orden 1186060307891929990 `shouldBe` 261

  describe "ordenEntre" $
    it "e1" $
      take 5 (ordenEntre 10 11)  `shouldBe`  [829,928,9059,9149,9239]

  describe "menorDeOrdenMayor" $ do
    it "e1" $
      menorDeOrdenMayor 2   `shouldBe`  19
    it "e2" $
      menorDeOrdenMayor 20  `shouldBe`  89

  describe "menoresdDeOrdenMayor" $
    it "e1" $
      menoresdDeOrdenMayor 5  `shouldBe`  [(1,10),(2,19),(3,59),(4,69),(5,79)]

  describe "prop_ordenDe196" $
    it "e1" $
      property prop_ordenDe196
