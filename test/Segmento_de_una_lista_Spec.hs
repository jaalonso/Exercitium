module Segmento_de_una_lista_Spec (main, spec) where

import Segmento_de_una_lista
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    segmento 3 4 [3,4,1,2,7,9,0]  `shouldBe`  [1,2]
  it "e2" $
    segmento 3 5 [3,4,1,2,7,9,0]  `shouldBe`  [1,2,7]
  it "e3" $
    segmento 5 3 [3,4,1,2,7,9,0]  `shouldBe`  []
