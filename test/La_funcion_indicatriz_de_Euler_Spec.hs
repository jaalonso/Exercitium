module La_funcion_indicatriz_de_Euler_Spec (main, spec) where

import La_funcion_indicatriz_de_Euler
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG phi = do
  it "e1" $
    map phi [10..20] `shouldBe` [4,10,4,12,6,8,8,16,6,18,8]

spec :: Spec
spec = do
  describe "def. 1" $ specG phi1
  describe "def. 2" $ specG phi2
  describe "def. 3" $ specG phi3
  describe "def. 4" $ specG phi4
  describe "equivalencia" $ it "p1" $ property prop_phi
