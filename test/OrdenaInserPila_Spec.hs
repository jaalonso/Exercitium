module OrdenaInserPila_Spec (main, spec) where

import OrdenaInserPila
import TAD.Pila
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Pila Int -> Pila Int) -> Spec
specG ordenaInserPila =
  it "e1" $
    show (ordenaInserPila (apila 4 (apila 1 (apila 3 vacia))))
    `shouldBe` "1 | 3 | 4"

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenaInserPila1
  describe "def. 2" $ specG ordenaInserPila2
  describe "equivalencia" $ it "p1" $ property prop_ordenaInserPila
