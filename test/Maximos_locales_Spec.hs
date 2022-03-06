module Maximos_locales_Spec (main, spec) where

import Maximos_locales
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (String -> String) -> Spec
specG maximosLocales = do
  it "e1" $
      maximosLocales "cbecgafb"  `shouldBe`  "egf"
  it "e2" $
      maximosLocales ['a'..'z']  `shouldBe`  []
  it "e3" $
      maximosLocales "adbpmqexyz" `shouldBe`  "dpq"

spec :: Spec
spec = do
  describe "def. 1" $ specG maximosLocales1
  describe "def. 2" $ specG maximosLocales2
