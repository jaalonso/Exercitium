-- Diagonales_principales.hs
-- Diagonales principales de una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-febrero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La lista de las diagonales principales de la matriz
--    1  2  3  4
--    5  6  7  8
--    9 10 11 12
-- es
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
--
-- Definir la función
--    diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
-- tal que (diagonalesPrincipales p) es la lista de las diagonales
-- principales de p. Por ejemplo,
--    λ> diagonalesPrincipales (listArray ((1,1),(3,4)) [1..12])
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
-- ---------------------------------------------------------------------

module Diagonales_principales where

import Data.Array (Array, (!), bounds, listArray)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

diagonalesPrincipales1 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales1 p =
  [[p ! ij | ij <- ijs] | ijs <- posicionesDiagonalesPrincipales1 m n]
  where (_,(m,n)) = bounds p

posicionesDiagonalesPrincipales1 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales1 m n =
  [extension ij | ij <- iniciales]
  where iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]]
        extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- 2ª solución
-- ===========

diagonalesPrincipales2 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales2 p =
  [[p ! ij | ij <- ijs] | ijs <- posicionesDiagonalesPrincipales2 m n]
  where (_,(m,n)) = bounds p

posicionesDiagonalesPrincipales2 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales2 m n =
  [zip [i..m] [1..n] | i <- [m,m-1..1]] ++
  [zip [1..m] [j..n] | j <- [2..n]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Array (Int,Int) Int -> [[Int]]) -> Spec
specG diagonalesPrincipales = do
  it "e1" $
    diagonalesPrincipales (listArray ((1,1),(3,4)) [1..12]) `shouldBe`
      [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]

spec :: Spec
spec = do
  describe "def. 1" $ specG diagonalesPrincipales1
  describe "def. 2" $ specG diagonalesPrincipales2

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_diagonalesPrincipales2 :: Positive Int -> Positive Int -> Bool
prop_diagonalesPrincipales2 (Positive m) (Positive n) =
  diagonalesPrincipales1 p == diagonalesPrincipales2 p
  where p = listArray ((1,1),(m,n)) [1..]

-- La comprobación es
--    λ> quickCheck prop_diagonalesPrincipales2
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (diagonalesPrincipales1 (listArray ((1,1),(10^4,10^4)) [1..]))
--    19999
--    (6.90 secs, 8,010,369,224 bytes)
--    λ> length (diagonalesPrincipales2 (listArray ((1,1),(10^4,10^4)) [1..]))
--    19999
--    (6.78 secs, 8,008,289,224 bytes)
