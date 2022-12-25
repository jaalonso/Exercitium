module Arboles_balanceados_Spec (main, spec) where

import Arboles_balanceados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    balanceado (N 5 H (N 3 H H))
    `shouldBe` True
  it "e2" $
    balanceado (N 4 (N 3 (N 2 H H) H) (N 5 H (N 6 H (N 7 H H))))
    `shouldBe` False
