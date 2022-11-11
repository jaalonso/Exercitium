module Reconocimiento_de_subcadenas_Spec (main, spec) where

import Reconocimiento_de_subcadenas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (String -> String -> Bool) -> Spec
specG esSubcadena = do
  it "e1" $
    esSubcadena "casa" "escasamente"   `shouldBe`  True
  it "e2" $
    esSubcadena "cante" "escasamente"  `shouldBe`  False
  it "e3" $
    esSubcadena "" ""                  `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG esSubcadena1
  describe "def. 2" $ specG esSubcadena2
  describe "def. 3" $ specG esSubcadena3
  describe "def. 4" $ specG esSubcadena4
  describe "def. 5" $ specG esSubcadena5
  describe "equivalencia" $ it "p1" $ property prop_esSubcadena
