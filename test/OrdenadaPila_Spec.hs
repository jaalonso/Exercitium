module OrdenadaPila_Spec (main, spec) where

import OrdenadaPila
import TAD.PilaConListas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Bool) -> Spec
specG ordenadaPila' = do
  it "e1" $
    ordenadaPila' (apila 1 (apila 5 (apila 6 vacia))) `shouldBe` True
  it "e2" $
    ordenadaPila' (apila 1 (apila 0 (apila 6 vacia))) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadaPila
  describe "def. 2" $ specG ordenadaPila2
  describe "equivalencia" $ it "p1" $ property prop_ordenadaPila
