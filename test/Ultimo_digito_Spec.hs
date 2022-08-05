module Ultimo_digito_Spec (main, spec) where

import Ultimo_digito
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    ultimoDigito 325  `shouldBe`  5
