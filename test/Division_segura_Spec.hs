module Division_segura_Spec (main, spec) where

import Division_segura
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Double -> Double -> Double) -> Spec
specG divisionSegura = do
  it "e1" $
    divisionSegura 7 2  `shouldBe`  3.5
  it "e2" $
    divisionSegura 7 0  `shouldBe`  9999.0

spec :: Spec
spec = do
  describe "def. 1" $ specG divisionSegura1
  describe "def. 2" $ specG divisionSegura2
