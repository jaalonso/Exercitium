module Media_aritmetica_de_tres_numeros_Spec (main, spec) where

import Media_aritmetica_de_tres_numeros
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    media3 1 3 8     `shouldBe`  4.0
  it "e2" $
    media3 (-1) 0 7  `shouldBe`  2.0
  it "e3" $
    media3 (-3) 0 3  `shouldBe`  0.0
