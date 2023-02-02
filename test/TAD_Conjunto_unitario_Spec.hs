module TAD_Conjunto_unitario_Spec (main, spec) where

import TAD_Conjunto_unitario
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    show (unitario 5) `shouldBe` "{5}"
