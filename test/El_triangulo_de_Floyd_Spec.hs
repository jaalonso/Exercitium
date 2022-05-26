module El_triangulo_de_Floyd_Spec (main, spec) where

import El_triangulo_de_Floyd
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([[Integer]]) -> Spec
specG trianguloFloyd = do
  it "e1" $
    take 4 trianguloFloyd `shouldBe`
    [[1],
     [2,3],
     [4,5,6],
     [7,8,9,10]]

spec :: Spec
spec = do
  describe "def. 1" $ specG trianguloFloyd1
  describe "def. 2" $ specG trianguloFloyd2
  describe "def. 3" $ specG trianguloFloyd3
  describe "def. 4" $ specG trianguloFloyd4
  describe "equivalencia" $ it "p1" $ property prop_trianguloFloyd
