module TAD_Union_de_varios_conjuntos_Spec (main, spec) where

import TAD_Union_de_varios_conjuntos
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Conj Int] -> Conj Int) -> Spec
specG unionG' = do
  it "e1" $
    show (unionG' [ej1, ej2, ej3]) `shouldBe` "{3, 5, 6}"
  where
    ej1 = inserta 3 (inserta 5 vacio)
    ej2 = inserta 5 (inserta 6 vacio)
    ej3 = inserta 3 (inserta 6 vacio)

spec :: Spec
spec = do
  describe "def. 1" $ specG unionG
  describe "def. 2" $ specG unionG2
