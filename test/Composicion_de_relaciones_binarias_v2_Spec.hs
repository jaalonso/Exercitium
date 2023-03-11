module Composicion_de_relaciones_binarias_v2_Spec (main, spec) where

import Composicion_de_relaciones_binarias_v2
import Relaciones_binarias
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Rel Int -> Rel Int -> Rel Int) -> Spec
specG composicion' =
  it "e1" $
    composicion' (R ([1,2],[(1,2),(2,2)])) (R ([1,2],[(2,1)]))
    `shouldBe` R ([1,2],[(1,1),(2,1)])

spec :: Spec
spec = do
  describe "def. 1" $ specG composicion
  describe "def. 2" $ specG composicion2
  describe "equivalencia" $ it "p1" $ property prop_composicion
