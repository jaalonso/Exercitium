module Aplanamiento_de_un_arbol_Spec (main, spec) where

import Aplanamiento_de_un_arbol
import Arboles_binarios (Arbol (..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    aplana (N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))) `shouldBe`
    [1,3,4,5,6,7,9]
