module ConjuntoConListasNoOrdenadasConDuplicados_Spec (main, spec) where

import TAD.ConjuntoConListasNoOrdenadasConDuplicados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (vacio :: Conj Int) `shouldBe` "{}"
  it "e2" $
    show (inserta 5 vacio) `shouldBe` "{5}"
  it "e3" $
    show (inserta 2 (inserta 5 vacio)) `shouldBe` "{2, 5}"
  it "e4" $
    show (inserta 5 (inserta 2 vacio)) `shouldBe` "{2, 5}"
  it "e5" $
    menor (inserta 5 (inserta 2 vacio)) `shouldBe` 2
  it "e6" $
    show (elimina 2 (inserta 5 (inserta 2 vacio))) `shouldBe` "{5}"
  it "e7" $
    pertenece 2 (inserta 5 (inserta 2 vacio)) `shouldBe` True
  it "e8" $
    pertenece 4 (inserta 5 (inserta 2 vacio)) `shouldBe` False
  it "e9" $
    esVacio (inserta 5 (inserta 2 vacio)) `shouldBe` False
  it "e10" $
    esVacio vacio `shouldBe` True
