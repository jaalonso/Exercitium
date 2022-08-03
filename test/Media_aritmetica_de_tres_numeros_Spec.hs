module Media_aritmetica_de_tres_numeros_Spec (main, spec) where

import Media_aritmetica_de_tres_numeros
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Float -> Float -> Float -> Float) -> Spec
specG media3 = do
  it "e1" $
    media3 1 3 8     `shouldBe`  4.0
  it "e2" $
    media3 (-1) 0 7  `shouldBe`  2.0
  it "e3" $
    media3 (-3) 0 3  `shouldBe`  0.0

spec :: Spec
spec = do
  describe "def. 1" $ specG media3a
  describe "def. 2" $ specG media3b
  describe "equivalencia" $ it "p1" $ property prop_media3
