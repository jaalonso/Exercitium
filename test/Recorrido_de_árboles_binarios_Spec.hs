module Recorrido_de_árboles_binarios_Spec (main, spec) where

import Recorrido_de_árboles_binarios
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    preorden1 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
  it "e2" $
    preorden2 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
  it "e3" $
    postorden1 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
  it "e4" $
    postorden2 (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
