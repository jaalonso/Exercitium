module Area_corona_circular_Spec (main, spec) where

import Area_corona_circular
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    areaDeCoronaCircular 1 2 `shouldBe` 9.42477796076938
  it "e2" $
    areaDeCoronaCircular 2 5 `shouldBe` 65.97344572538566
  it "e3" $
    areaDeCoronaCircular 3 5 `shouldBe` 50.26548245743669
