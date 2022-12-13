module Valor_de_la_resta_Spec (main, spec) where

import Valor_de_la_resta
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    resta (Lit 42) (Lit 2) `shouldBe`  Suma (Lit 42) (Op (Lit 2))
  it "p1" $ property prop_resta
