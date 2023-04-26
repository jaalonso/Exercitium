module Expresiones_aritmeticas_reducibles_Spec (main, spec) where

import Expresiones_aritmeticas_reducibles
import Expresion_aritmetica_con_variables (Expr (C, V, S, P))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    reducible (S (C 3) (C 4))             `shouldBe` True
  it "e2" $
    reducible (S (C 3) (V 'x'))           `shouldBe` False
  it "e3" $
    reducible (S (C 3) (P (C 4) (C 5)))   `shouldBe` True
  it "e4" $
    reducible (S (V 'x') (P (C 4) (C 5))) `shouldBe` True
  it "e5" $
    reducible (S (C 3) (P (V 'x') (C 5))) `shouldBe` False
  it "e6" $
    reducible (C 3)                       `shouldBe` False
  it "e7" $
    reducible (V 'x')                     `shouldBe` False
