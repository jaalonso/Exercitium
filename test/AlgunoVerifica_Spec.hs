module AlgunoVerifica_Spec (main, spec) where

import AlgunoVerifica
import TAD.Cola
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Cola Int -> Bool) -> Spec
specG algunoVerifica = do
  it "e1" $
    algunoVerifica (< 0) (inserta 3 (inserta (-2) vacia)) `shouldBe` True
  it "e2" $
    algunoVerifica (< 0) (inserta 3 (inserta 2 vacia))    `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG algunoVerifica1
  describe "def. 2" $ specG algunoVerifica2
