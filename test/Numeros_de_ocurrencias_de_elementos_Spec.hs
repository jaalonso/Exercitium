module Numeros_de_ocurrencias_de_elementos_Spec (main, spec) where

import Numeros_de_ocurrencias_de_elementos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (String -> [(Char,Int)]) -> Spec
specG ocurrenciasElementos = do
  it "e1" $
    ocurrenciasElementos "abracadabra" `shouldBe`
    [('a',5),('b',2),('r',2),('c',1),('d',1)]
  it "e2" $
    ocurrenciasElementos "Code Wars"   `shouldBe`
    [('C',1),('o',1),('d',1),('e',1),(' ',1),('W',1),('a',1),('r',1),('s',1)]


spec :: Spec
spec = do
  describe "def. 1" $ specG ocurrenciasElementos1
  describe "def. 2" $ specG ocurrenciasElementos2
  describe "def. 3" $ specG ocurrenciasElementos3
  describe "def. 4" $ specG ocurrenciasElementos4
