module El_tipo_de_las_listas_Spec (main, spec) where

import El_tipo_de_las_listas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  it "e1" $
    longitud (Cons 24 (Cons 2 (Cons 5 Nil)))  `shouldBe`  3
