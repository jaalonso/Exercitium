module Particiones_en_k_subconjuntos_Spec (main, spec) where

import Particiones_en_k_subconjuntos
import Data.List (sort)
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Int] -> Int -> [[[Int]]]) -> Spec
specG particiones = do
  it "e1" $
    particiones [2,3,6] 2 `iguales` 
    [[[2],[3,6]],[[2,3],[6]],[[3],[2,6]]]
  it "e2" $
    particiones [2,3,6] 3 `iguales` 
    [[[2],[3],[6]]]
  it "e3" $
    particiones [4,2,3,6] 3 `iguales` 
    [[[4],[2],[3,6]],[[4],[2,3],[6]],[[4],[3],[2,6]],
     [[4,2],[3],[6]],[[2],[4,3],[6]],[[2],[3],[4,6]]]
  it "e4" $
    particiones [4,2,3,6] 1 `iguales` 
    [[[4,2,3,6]]]
  it "e5" $
    particiones [4,2,3,6] 4 `iguales` 
    [[[4],[2],[3],[6]]]
  where
    iguales xss yss = sort (map sort [map sort x | x <- xss]) ==
                      sort (map sort [map sort y | y <- yss])

  
spec :: Spec
spec = do
  describe "def. 1" $ specG particiones1
  describe "def. 2" $ specG particiones2
  describe "def. 3" $ specG particiones3
