module Duplicacion_de_cada_elemento_Spec (main, spec) where

import Duplicacion_de_cada_elemento
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (String -> String) -> Spec
specG duplicaElementos = do
  it "e1" $
    duplicaElementos "abcd" `shouldBe` "aabbccdd"
  it "e2" $
    duplicaElementos "Adidas" `shouldBe` "AAddiiddaass"
  it "e3" $
    duplicaElementos "1337" `shouldBe` "11333377"
  it "e4" $
    duplicaElementos "illuminati" `shouldBe` "iilllluummiinnaattii"
  it "e5" $
    duplicaElementos "123456" `shouldBe` "112233445566"
  it "e6" $
    duplicaElementos "%^&*(" `shouldBe` "%%^^&&**(("

spec :: Spec
spec = do
  describe "def. 1" $ specG duplicaElementos1
  describe "def. 2" $ specG duplicaElementos2
  describe "def. 3" $ specG duplicaElementos3
  describe "def. 4" $ specG duplicaElementos4
  describe "def. 5" $ specG duplicaElementos5
  describe "def. 6" $ specG duplicaElementos6
