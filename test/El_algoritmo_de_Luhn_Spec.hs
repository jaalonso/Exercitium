module El_algoritmo_de_Luhn_Spec (main, spec) where

import El_algoritmo_de_Luhn
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> Bool) -> Spec
specG luhn = do
  it "e1" $
    luhn 5594589764218858  `shouldBe`  True
  it "e2" $
    luhn 1234567898765432  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG luhn1
  describe "def. 2" $ specG luhn2
