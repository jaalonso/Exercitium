module TAD_subconjuntoPropio_Spec (main, spec) where

import TAD_subconjuntoPropio
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    subconjuntoPropio ej1 ej2 `shouldBe` True
  it "e2" $
    subconjuntoPropio ej1 ej3 `shouldBe` False
  it "e3" $
    subconjuntoPropio ej1 ej4 `shouldBe` False
  where
    ej1 = inserta 5 (inserta 2 vacio)
    ej2 = inserta 3 (inserta 2 (inserta 5 vacio))
    ej3 = inserta 3 (inserta 4 (inserta 5 vacio))
    ej4 = inserta 2 (inserta 5 vacio)
