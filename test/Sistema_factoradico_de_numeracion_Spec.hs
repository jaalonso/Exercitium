module Sistema_factoradico_de_numeracion_Spec (main, spec) where

import Sistema_factoradico_de_numeracion
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (String -> Integer) -> (Integer -> String) -> Spec
specG factoradicoAdecimal decimalAfactoradico = do
  it "e1" $
    decimalAfactoradico 463      `shouldBe` "341010"
  it "e2" $
    decimalAfactoradico 8999     `shouldBe` "15243210"
  it "e3" $
    decimalAfactoradico 2982     `shouldBe` "4041000"
  it "e4" $
    decimalAfactoradico 36288000 `shouldBe` "A0000000000"
  it "e5" $
    factoradicoAdecimal "341010" `shouldBe` 463
  it "e6" $
    factoradicoAdecimal "15243210" `shouldBe` 8999

spec :: Spec
spec = do
  describe "def. 1" $ specG factoradicoAdecimal1 decimalAfactoradico1
  describe "def. 2" $ specG factoradicoAdecimal2 decimalAfactoradico2
  describe "def. 3" $ specG factoradicoAdecimal3 decimalAfactoradico3
  describe "def. 4" $ specG factoradicoAdecimal4 decimalAfactoradico4
