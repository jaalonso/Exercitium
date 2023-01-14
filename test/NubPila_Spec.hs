module NubPila_Spec (main, spec) where

import NubPila
import TAD.PilaConListas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Pila Int) -> Spec
specG nubPila =
  it "e1" $
    show (nubPila (apila 3 (apila 1 (apila 3 (apila 5 vacia)))))
    `shouldBe` "1 | 3 | 5"

spec :: Spec
spec = do
  describe "def. 1" $ specG nubPila1
  describe "def. 2" $ specG nubPila2
  describe "equivalencia" $ it "p1" $ property prop_nubPila
