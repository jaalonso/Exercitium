module Pol_Crea_termino_Spec (main, spec) where

import Pol_Crea_termino
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    show (creaTermino 2 5) `shouldBe` "5*x^2"
