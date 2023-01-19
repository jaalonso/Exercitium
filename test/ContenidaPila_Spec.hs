module ContenidaPila_Spec (main, spec) where

import ContenidaPila
import TAD.Pila
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Pila Int -> Bool) -> Spec
specG contenidaPila = do
  it "e1" $
    contenidaPila ej1 ej3 `shouldBe` True
  it "e2" $
    contenidaPila ej2 ej3 `shouldBe` False
  where
    ej1 = apila 3 (apila 2 vacia)
    ej2 = apila 3 (apila 4 vacia)
    ej3 = apila 5 (apila 2 (apila 3 vacia))


spec :: Spec
spec = do
  describe "def. 1" $ specG contenidaPila1
  describe "def. 2" $ specG contenidaPila2
