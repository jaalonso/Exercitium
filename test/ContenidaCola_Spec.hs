module ContenidaCola_Spec (main, spec) where

import ContenidaCola
import TAD.Cola
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Cola Int -> Cola Int -> Bool) -> Spec
specG contenidaCola = do
  it "e1" $
    contenidaCola ej1 ej3 `shouldBe` True
  it "e2" $
    contenidaCola ej2 ej3 `shouldBe` False
  where
    ej1 = inserta 3 (inserta 2 vacia)
    ej2 = inserta 3 (inserta 4 vacia)
    ej3 = inserta 5 (inserta 2 (inserta 3 vacia))

spec :: Spec
spec = do
  describe "def. 1" $ specG contenidaCola1
  describe "def. 2" $ specG contenidaCola2
