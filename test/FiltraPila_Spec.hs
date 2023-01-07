module FiltraPila_Spec (main, spec) where

import FiltraPila
import TAD.PilaConListas
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Pila Int -> Pila Int) -> Spec
specG filtraPila = do
  it "e1" $
    show (filtraPila even ejPila) `shouldBe` "6 | 4"
  it "e2" $
    show (filtraPila odd ejPila) `shouldBe` "3 | 1"
  where ejPila = apila 6 (apila 3 (apila 1 (apila 4 vacia)))

spec :: Spec
spec = do
  describe "def. 1" $ specG filtraPila1
  describe "def. 2" $ specG filtraPila2
