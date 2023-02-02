module TAD_Numero_de_elementos_de_un_conjunto_Spec (main, spec) where

import TAD_Numero_de_elementos_de_un_conjunto
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Conj Int -> Int) -> Spec
specG cardinal' = do
  it "e1" $
    cardinal' (inserta 4 (inserta 5 vacio))             `shouldBe` 2
  it "e2" $
    cardinal' (inserta 4 (inserta 5 (inserta 4 vacio))) `shouldBe` 2

spec :: Spec
spec = do
  describe "def. 1" $ specG cardinal
  describe "def. 2" $ specG cardinal2
  describe "equivalencia" $ it "p1" $ property prop_cardinal
