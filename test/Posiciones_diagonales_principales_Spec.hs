module Posiciones_diagonales_principales_Spec (main, spec) where

import Posiciones_diagonales_principales
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> Int -> [[(Int, Int)]]) -> Spec
specG posicionesDiagonalesPrincipales = do
  it "e1" $
    posicionesDiagonalesPrincipales 3 4 `shouldBe`
    [[(3,1)],
     [(2,1),(3,2)],
     [(1,1),(2,2),(3,3)],
     [(1,2),(2,3),(3,4)],
     [(1,3),(2,4)],
     [(1,4)]]
  it "e2" $
    posicionesDiagonalesPrincipales 4 4 `shouldBe`
    [[(4,1)],
     [(3,1),(4,2)],
     [(2,1),(3,2),(4,3)],
     [(1,1),(2,2),(3,3),(4,4)],
     [(1,2),(2,3),(3,4)],
     [(1,3),(2,4)],
     [(1,4)]]
  it "e3" $
    posicionesDiagonalesPrincipales 4 3 `shouldBe`
    [[(4,1)],
     [(3,1),(4,2)],
     [(2,1),(3,2),(4,3)],
     [(1,1),(2,2),(3,3)],
     [(1,2),(2,3)],
     [(1,3)]]

spec :: Spec
spec = do
  describe "def. 1" $ specG posicionesDiagonalesPrincipales1
  describe "def. 2" $ specG posicionesDiagonalesPrincipales2
