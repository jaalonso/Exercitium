module Arbol_de_factorizacion_Spec (main, spec) where

import Arbol_de_factorizacion
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Arbol) -> Spec
specG arbolFactorizacion = do
  it "e1" $
    arbolFactorizacion 60  `shouldBe` N 60 (N 6 (H 2) (H 3)) (N 10 (H 2) (H 5))
  it "e2" $
    arbolFactorizacion 45  `shouldBe` N 45 (H 5) (N 9 (H 3) (H 3))
  it "e3" $
    arbolFactorizacion 7   `shouldBe` H 7
  it "e4" $
    arbolFactorizacion 9   `shouldBe` N 9 (H 3) (H 3)
  it "e5" $
    arbolFactorizacion 14  `shouldBe` N 14 (H 2) (H 7)
  it "e6" $
    arbolFactorizacion 28  `shouldBe` N 28 (N 4 (H 2) (H 2)) (H 7)
  it "e7" $
    arbolFactorizacion 84  `shouldBe` N 84 (H 7) (N 12 (H 3) (N 4 (H 2) (H 2)))

spec :: Spec
spec = do
  describe "def. 1" $ specG arbolFactorizacion1
  describe "def. 2" $ specG arbolFactorizacion2
  describe "equivalencia" $ it "p1" $ property prop_arbolFactorizacion
