module La_serie_de_Thue_Morse_Spec (main, spec) where

import La_serie_de_Thue_Morse
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([[Int]]) -> Spec
specG serieThueMorse = 
  it "e1" $
    take 4 serieThueMorse `shouldBe`
    [[0],[0,1],[0,1,1,0],[0,1,1,0,1,0,0,1]]


spec :: Spec
spec = do
  describe "def. 1" $ specG serieThueMorse1
  describe "def. 2" $ specG serieThueMorse2
  describe "def. 3" $ specG serieThueMorse3
  describe "def. 4" $ specG serieThueMorse4
