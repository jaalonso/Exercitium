module Representacion_matricial_de_relaciones_binarias_Spec (main, spec) where

import Representacion_matricial_de_relaciones_binarias
import Data.Array (elems)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Relacion -> Matriz) -> Spec
specG matrizRB = do
  it "e1" $
    elems (matrizRB ([1..3],[(1,1), (1,3), (3,1), (3,3)]))
    `shouldBe` [True,False,True,False,False,False,True,False,True]
  it "e2" $
    elems (matrizRB ([1..3],[(1,3), (3,1)]))
    `shouldBe` [False,False,True,False,False,False,True,False,False]

spec :: Spec
spec = do
  describe "def. 1" $ specG matrizRB1
  describe "def. 2" $ specG matrizRB2
  describe "def. 3" $ specG matrizRB3
  describe "equivalencia" $ modifyMaxSize (const 7) $ it "p1" $ property prop_matrizRB
