-- Diagonales_principales.hs
-- Diagonales principales de una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-Febrero-2015 (actualizado 3-Febrero-2026)
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

import Data.Array (Array, (!), bounds, indices, listArray)
import Data.Matrix (submatrix, getDiag, matrix)
import qualified Data.Vector as V (toList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

diagonalesPrincipales1 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales1 p =
    [ [p ! (i, j) | (i, j) <- indices p, j - i == k]
    | k <- [1-m..n-1] ]
  where
    (_, (m,n)) = bounds p

-- 2ª solución
-- ===========

diagonalesPrincipales2 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales2 p =
  [[p ! ij | ij <- ijs] | ijs <- posicionesDiagonalesPrincipales2 m n]
  where (_,(m,n)) = bounds p

posicionesDiagonalesPrincipales2 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales2 m n =
  [extension ij | ij <- iniciales]
  where iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]]
        extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- 3ª solución
-- ===========

diagonalesPrincipales3 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales3 p =
  [[p ! ij | ij <- ijs] | ijs <- posicionesDiagonalesPrincipales3 m n]
  where (_,(m,n)) = bounds p

posicionesDiagonalesPrincipales3 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales3 m n =
  [zip [i..m] [1..n] | i <- [m,m-1..1]] ++
  [zip [1..m] [j..n] | j <- [2..n]]

-- 4ª solución
-- ===========

diagonalesPrincipales4 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales4 p =
    [ [p ! (i, i + k) | i <- [max 1 (1 - k) .. min m (n - k)]]
    | k <- [1-m..n-1]
    ]
  where (_,(m,n)) = bounds p

-- 5ª solución
-- ===========

diagonalesPrincipales5 :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales5 p = diagsInferiores ++ diagsSuperiores
  where
    (_,(m,n)) = bounds p
    p' = matrix m n (\(i, j) -> p ! (i,j))
    diagsInferiores =
        [ V.toList (getDiag (submatrix i m 1 n p'))
        | i <- [m, m-1 .. 2] ]
    diagsSuperiores =
        [ V.toList (getDiag (submatrix 1 m j n p'))
        | j <- [1 .. n] ]

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
  describe "def. 3" $ specG diagonalesPrincipales3
  describe "def. 4" $ specG diagonalesPrincipales4
  describe "def. 5" $ specG diagonalesPrincipales5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_equivalencia :: Positive Int -> Positive Int -> Bool
prop_equivalencia (Positive m) (Positive n) =
  all (== diagonalesPrincipales1 p)
      [ diagonalesPrincipales2 p
      , diagonalesPrincipales3 p
      , diagonalesPrincipales4 p
      , diagonalesPrincipales5 p
      ]
  where p = listArray ((1,1),(m,n)) [1..]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> sum (map sum (diagonalesPrincipales1 (listArray ((1,1),(150,150)) [1..])))
--    253136250
--    (2.66 secs, 1,363,876,192 bytes)
--    λ> sum (map sum (diagonalesPrincipales2 (listArray ((1,1),(150,150)) [1..])))
--    253136250
--    (0.06 secs, 15,509,224 bytes)
--    λ> sum (map sum (diagonalesPrincipales3 (listArray ((1,1),(150,150)) [1..])))
--    253136250
--    (0.05 secs, 13,780,176 bytes)
--    λ> sum (map sum (diagonalesPrincipales4 (listArray ((1,1),(150,150)) [1..])))
--    253136250
--    (0.04 secs, 12,516,384 bytes)
--    λ> sum (map sum (diagonalesPrincipales5 (listArray ((1,1),(150,150)) [1..])))
--    253136250
--    (0.04 secs, 13,399,320 bytes)
--
--    λ> sum (map sum (diagonalesPrincipales2 (listArray ((1,1),(1500,1500)) [1..])))
--    2531251125000
--    (2.01 secs, 1,461,757,616 bytes)
--    λ> sum (map sum (diagonalesPrincipales3 (listArray ((1,1),(1500,1500)) [1..])))
--    2531251125000
--    (1.36 secs, 1,298,665,880 bytes)
--    λ> sum (map sum (diagonalesPrincipales4 (listArray ((1,1),(1500,1500)) [1..])))
--    2531251125000
--    (1.59 secs, 1,172,629,632 bytes)
--    λ> sum (map sum (diagonalesPrincipales5 (listArray ((1,1),(1500,1500)) [1..])))
--    2531251125000
--    (1.42 secs, 1,262,475,584 bytes)
