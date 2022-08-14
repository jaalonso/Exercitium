module Intercambio_de_componentes_de_un_par_Spec (main, spec) where

import Intercambio_de_componentes_de_un_par
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    intercambia (2,5)  `shouldBe`  (5,2)
  it "e2" $
    intercambia (5,2)  `shouldBe`  (2,5)
