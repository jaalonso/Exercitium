module La_sucesion_del_reloj_astronomico_de_Praga_Spec (main, spec) where

import La_sucesion_del_reloj_astronomico_de_Praga
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG reloj = do
  it "e1" $
    take 11 reloj
    `shouldBe` [1,2,3,4,32,123,43,2123,432,1234,32123]

spec :: Spec
spec = do
  describe "def. 1" $ specG reloj1
  describe "def. 2" $ specG reloj2
  describe "equivalencia" $ it "p1" $ property prop_reloj
