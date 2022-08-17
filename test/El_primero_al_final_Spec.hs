module El_primero_al_final_Spec (main, spec) where

import El_primero_al_final
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG rota1 = do
  it "e1" $
    rota1 [3,2,5,7]  `shouldBe`  [2,5,7,3]

spec :: Spec
spec = do
  describe "def. 1" $ specG rota1a
  describe "def. 2" $ specG rota1b
  describe "equivalencia" $ it "p1" $ property prop_rota1
