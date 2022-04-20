module Elimina_aisladas_Spec (main, spec) where

import Elimina_aisladas
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Char -> String -> String) -> Spec
specG eliminaAisladas = do
  it "e1" $
    eliminaAisladas 'X' ""                  `shouldBe` ""
  it "e2" $
    eliminaAisladas 'X' "X"                 `shouldBe` ""
  it "e3" $
    eliminaAisladas 'X' "XX"                `shouldBe` "XX"
  it "e4" $
    eliminaAisladas 'X' "XXX"               `shouldBe` "XXX"
  it "e5" $
    eliminaAisladas 'X' "abcd"              `shouldBe` "abcd"
  it "e6" $
    eliminaAisladas 'X' "Xabcd"             `shouldBe` "abcd"
  it "e7" $
    eliminaAisladas 'X' "XXabcd"            `shouldBe` "XXabcd"
  it "e8" $
    eliminaAisladas 'X' "XXXabcd"           `shouldBe` "XXXabcd"
  it "e9" $
    eliminaAisladas 'X' "abcdX"             `shouldBe` "abcd"
  it "e10" $
    eliminaAisladas 'X' "abcdXX"            `shouldBe` "abcdXX"
  it "e11" $
    eliminaAisladas 'X' "abcdXXX"           `shouldBe` "abcdXXX"
  it "e12" $
    eliminaAisladas 'X' "abXcd"             `shouldBe` "abcd"
  it "e13" $
    eliminaAisladas 'X' "abXXcd"            `shouldBe` "abXXcd"
  it "e14" $
    eliminaAisladas 'X' "abXXXcd"           `shouldBe` "abXXXcd"
  it "e15" $
    eliminaAisladas 'X' "XabXcdX"           `shouldBe` "abcd"
  it "e16" $
    eliminaAisladas 'X' "XXabXXcdXX"        `shouldBe` "XXabXXcdXX"
  it "e17" $
    eliminaAisladas 'X' "XXXabXXXcdXXX"     `shouldBe` "XXXabXXXcdXXX"
  it "e18" $
    eliminaAisladas 'X' "XabXXcdXeXXXfXx"   `shouldBe` "abXXcdeXXXfx"

spec :: Spec
spec = do
  describe "def. 1" $ specG eliminaAisladas1
  describe "def. 2" $ specG eliminaAisladas2
  describe "def. 3" $ specG eliminaAisladas3
  describe "def. 4" $ specG eliminaAisladas4
  describe "equivalencia" $ it "p1" $ property prop_eliminaAisladas
