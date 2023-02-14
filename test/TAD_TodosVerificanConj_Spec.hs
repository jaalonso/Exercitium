module TodosVerificanConj_Spec (main, spec) where

import TAD_TodosVerificanConj
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Conj Int -> Bool) -> Spec
specG todos' = do
  it "e1" $
    todos' even (inserta 4 (inserta 6 vacio)) `shouldBe` True
  it "e2" $
    todos' even (inserta 4 (inserta 7 vacio)) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG todos
  describe "def. 2" $ specG todos2
