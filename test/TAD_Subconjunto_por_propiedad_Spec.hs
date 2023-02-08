module TAD_Subconjunto_por_propiedad_Spec (main, spec) where

import TAD_Subconjunto_por_propiedad
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Conj Int -> Conj Int) -> Spec
specG filtra' = do
  it "e1" $
    show (filtra' even (inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio)))))
      `shouldBe` "{2, 4}"
  it "e2" $
    show (filtra' odd (inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio)))))
      `shouldBe` "{5, 7}"


spec :: Spec
spec = do
  describe "def. 1" $ specG filtra
  describe "def. 2" $ specG filtra2
