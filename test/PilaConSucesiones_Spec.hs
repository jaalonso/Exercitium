module PilaConSucesiones_Spec (main, spec) where

import TAD.PilaConSucesiones
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (apila 4 (apila 3 (apila 2 (apila 5 vacia)))) `shouldBe` "4 | 3 | 2 | 5"
  it "e2" $
    show (vacia :: Pila Int) `shouldBe` "-"
  it "e4" $
    cima (apila 4 (apila 3 (apila 2 (apila 5 vacia)))) `shouldBe` 4
  it "e5" $
    show (desapila (apila 4 (apila 3 (apila 2 (apila 5 vacia))))) `shouldBe` "3 | 2 | 5"
  it "e6" $
    esVacia (apila 4 (apila 3 (apila 2 (apila 5 vacia)))) `shouldBe` False
  it "e7" $
    esVacia vacia `shouldBe` True
