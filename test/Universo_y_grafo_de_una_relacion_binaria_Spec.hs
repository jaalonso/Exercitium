module Universo_y_grafo_de_una_relacion_binaria_Spec (main, spec) where

import Universo_y_grafo_de_una_relacion_binaria
import Relaciones_binarias
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    universo r `shouldBe` [1,3]
  it "e2" $
    grafo r `shouldBe` [(3, 1), (3, 3)]
  where
    r = R ([1, 3],[(3, 1), (3, 3)])
