module TAD_AlgunosVerificanConj_Spec (main, spec) where

import TAD_AlgunosVerificanConj
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Conj Int -> Bool) -> Spec
specG algunos' = do
  it "e1" $
    algunos' even (inserta 4 (inserta 7 vacio)) `shouldBe` True
  it "e2" $
    algunos' even (inserta 3 (inserta 7 vacio)) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG algunos
  describe "def. 2" $ specG algunos2
