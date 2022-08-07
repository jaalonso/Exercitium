module Reconocimiento_de_palindromos_Spec (main, spec) where

import Reconocimiento_de_palindromos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    palindromo [3,2,5,2,3]    `shouldBe`  True
  it "e2" $
    palindromo [3,2,5,6,2,3]  `shouldBe`  False
