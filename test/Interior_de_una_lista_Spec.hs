module Interior_de_una_lista_Spec (main, spec) where

import Interior_de_una_lista
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    interior [2,5,3,7,3]  `shouldBe`  [5,3,7]
  it "e2" $
    interior [2..7]       `shouldBe`  [3,4,5,6]
