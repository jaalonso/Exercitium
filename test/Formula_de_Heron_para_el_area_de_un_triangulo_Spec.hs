module Formula_de_Heron_para_el_area_de_un_triangulo_Spec (main, spec) where

import Formula_de_Heron_para_el_area_de_un_triangulo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    area 3 4 5  `shouldBe`  6.0
