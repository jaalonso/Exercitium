module Posiciones_de_un_caracter_en_una_cadena_Spec (main, spec) where

import Posiciones_de_un_caracter_en_una_cadena
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Char -> String -> [Int]) -> Spec
specG posiciones =
  it "e1" $
    posiciones 'a' "Salamamca" `shouldBe` [1,3,5,8]

spec :: Spec
spec = do
  describe "def. 1" $ specG posiciones1
  describe "def. 2" $ specG posiciones2
  describe "def. 3" $ specG posiciones3
  describe "equivalencia" $ it "p1" $ property prop_posiciones
