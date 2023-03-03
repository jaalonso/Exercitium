module Universo_y_grafo_de_una_relacion_binaria_Spec (main, spec) where

import Universo_y_grafo_de_una_relacion_binaria
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    universo r `shouldBe` [1,2,3,4,5,6,7,8,9]
  it "e2" $
    grafo r `shouldBe` [(1,3),(2,6),(8,9),(2,7)]
  where
    r = ([1..9],[(1,3),(2,6),(8,9),(2,7)])
