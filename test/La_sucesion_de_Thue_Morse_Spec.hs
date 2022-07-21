module La_sucesion_de_Thue_Morse_Spec (main, spec) where

import La_sucesion_de_Thue_Morse
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Int]) -> Spec
specG sucThueMorse = do
  it "e1" $
    take 30 sucThueMorse `shouldBe`
    [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0]

spec :: Spec
spec = do
  describe "def. 1" $ specG sucThueMorse1
  describe "def. 2" $ specG sucThueMorse2
  describe "def. 3" $ specG sucThueMorse3
  describe "def. 4" $ specG sucThueMorse4
  describe "def. 5" $ specG sucThueMorse5
  describe "equivalencia" $ it "p1" $ property prop_sucThueMorse
