module Sumas_de_dos_abundantes_Spec (main, spec) where

import Sumas_de_dos_abundantes
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: [Integer] -> Spec
specG sumasDeDosAbundantes = do
  it "e1" $
    take 10 sumasDeDosAbundantes  `shouldBe`  [24,30,32,36,38,40,42,44,48,50]

spec :: Spec
spec = do
  describe "def. 1" $ specG sumasDeDosAbundantes1
  describe "def. 2" $ specG sumasDeDosAbundantes2
  describe "equivalencia" $ it "p1" $ property prop_sumasDeDosAbundantes
