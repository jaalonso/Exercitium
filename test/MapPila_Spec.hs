module MapPila_Spec (main, spec) where

import MapPila
import TAD.Pila
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Int) -> Pila Int -> Pila Int) -> Spec
specG mapPila =
  it "e1" $
    show (mapPila (+1) (apila 5 (apila 2 (apila 7 vacia))))
    `shouldBe` "6 | 3 | 8"

spec :: Spec
spec = do
  describe "def. 1" $ specG mapPila1
  describe "def. 2" $ specG mapPila2
