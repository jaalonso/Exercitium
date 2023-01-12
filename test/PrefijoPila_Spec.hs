module PrefijoPila_Spec (main, spec) where

import PrefijoPila
import TAD.PilaConListas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Pila Int -> Bool) -> Spec
specG prefijoPila = do
  it "e1" $
    prefijoPila ej1 ej2 `shouldBe` True
  it "e2" $
    prefijoPila ej1 ej3 `shouldBe` False
  where
    ej1 = apila 4 (apila 2 vacia)
    ej2 = apila 4 (apila 2 (apila 5 vacia))
    ej3 = apila 5 (apila 4 (apila 2 vacia))

spec :: Spec
spec = do
  describe "def. 1" $ specG prefijoPila1
  describe "def. 2" $ specG prefijoPila2
  describe "equivalencia" $ it "p1" $ property prop_prefijoPila
