module ColaConSucesiones_Spec (main, spec) where

import TAD.ColaConSucesiones
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (vacia :: Cola Int) `shouldBe` "-"
  it "e2" $
    show (inserta 5 (inserta 2 (inserta 3 vacia))) `shouldBe` "3 | 2 | 5"
  it "e3" $
    primero (inserta 5 (inserta 2 (inserta 3 vacia))) `shouldBe` 3
  it "e4" $
    show (resto (inserta 5 (inserta 2 (inserta 3 vacia)))) `shouldBe` "2 | 5"
  it "e5" $
    esVacia (inserta 5 (inserta 2 (inserta 3 vacia))) `shouldBe` False
  it "e6" $
    esVacia vacia `shouldBe` True
