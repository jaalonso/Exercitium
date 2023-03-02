module TAD_Subconjuntos_de_un_conjunto_Spec (main, spec) where

import TAD_Subconjuntos_de_un_conjunto
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Conj (Conj Int)) -> Spec
specG potencia' = do
  it "e1" $
    show (potencia' (inserta 1 (inserta 2 vacio)))
    `shouldBe` "{{}, {2}, {1}, {1, 2}}"
  it "e2" $
    show (potencia' (inserta 3 (inserta 1 (inserta 2 vacio))))
    `shouldBe` "{{}, {3}, {2}, {2, 3}, {1}, {1, 3}, {1, 2}, {1, 2, 3}}"

spec :: Spec
spec = do
  describe "def. 1" $ specG potencia
  describe "def. 2" $ specG potencia2
