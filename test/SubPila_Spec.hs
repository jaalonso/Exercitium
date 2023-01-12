module SubPila_Spec (main, spec) where

import SubPila
import TAD.PilaConListas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Pila Int -> Bool) -> Spec
specG subPila = do
  it "e1" $
    subPila ej1 ej2 `shouldBe` True
  it "e2" $
    subPila ej1 ej3 `shouldBe` False
  where
    ej1 = apila 2 (apila 3 vacia)
    ej2 = apila 7 (apila 2 (apila 3 (apila 5 vacia)))
    ej3 = apila 2 (apila 7 (apila 3 (apila 5 vacia)))

spec :: Spec
spec = do
  describe "def. 1" $ specG subPila1
  describe "def. 2" $ specG subPila2
  describe "equivalencia" $ it "p1" $ property prop_subPila
