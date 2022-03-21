-- Pares_adyacentes_iguales.hs
-- Número de pares de elementos adyacentes iguales en una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una matriz se puede representar mediante una lista de listas. Por
-- ejemplo, la matriz
--    |2 1 5|
--    |4 3 7|
-- se puede representar mediante la lista
--    [[2,1,5],[4,3,7]]
--
-- Definir la función
--    numeroParesAdyacentesIguales :: Eq a => [[a]] -> Int
-- tal que (numeroParesAdyacentesIguales xss) es el número de pares de
-- elementos consecutivos (en la misma fila o columna) iguales de la
-- matriz xss. Por ejemplo,
--    numeroParesAdyacentesIguales [[0,1],[0,2]]              ==  1
--    numeroParesAdyacentesIguales [[0,0],[1,2]]              ==  1
--    numeroParesAdyacentesIguales [[0,1],[0,0]]              ==  2
--    numeroParesAdyacentesIguales [[1,2],[1,4],[4,4]]        ==  3
--    numeroParesAdyacentesIguales ["ab","aa"]                ==  2
--    numeroParesAdyacentesIguales [[0,0,0],[0,0,0],[0,0,0]]  ==  12
--    numeroParesAdyacentesIguales [[0,0,0],[0,1,0],[0,0,0]]  ==  8
-- ---------------------------------------------------------------------

module Pares_adyacentes_iguales where

import Data.List (group,transpose)
import Data.Array ((!), listArray)
import Test.QuickCheck

-- 1ª solución
-- ===========

numeroParesAdyacentesIguales1 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales1 xss =
  numeroParesAdyacentesIgualesFilas xss +
  numeroParesAdyacentesIgualesFilas (transpose xss)

-- (numeroParesAdyacentesIgualesFilas xss) es el número de pares de
-- elementos consecutivos (en la misma fila) iguales de la matriz
-- xss. Por ejemplo,
--    λ> numeroParesAdyacentesIgualesFilas [[0,0,1,0],[0,1,1,0],[0,1,0,1]]
--    2
--    λ> numeroParesAdyacentesIgualesFilas ["0010","0110","0101"]
--    2
numeroParesAdyacentesIgualesFilas :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas xss =
  sum [numeroParesAdyacentesIgualesFila xs | xs <- xss]

-- La función anterior se puede definir con map
numeroParesAdyacentesIgualesFilas2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas2 xss =
  sum (map numeroParesAdyacentesIgualesFila xss)

-- y también se puede definir sin argumentos:
numeroParesAdyacentesIgualesFilas3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas3 =
  sum . (map numeroParesAdyacentesIgualesFila)

-- (numeroParesAdyacentesIgualesFila xs) es el número de pares de
-- elementos consecutivos de la lista xs. Por ejemplo,
--    numeroParesAdyacentesIgualesFila [5,5,5,2,5] ==  2
numeroParesAdyacentesIgualesFila :: Eq a => [a] -> Int
numeroParesAdyacentesIgualesFila xs =
  length [(x,y) | (x,y) <- zip xs (tail xs), x == y]

-- 2ª solución
-- ===========

numeroParesAdyacentesIguales2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales2 xss =
  length [(i,j) | i <- [1..m-1], j <- [1..n], p!(i,j) == p!(i+1,j)] +
  length [(i,j) | i <- [1..m], j <- [1..n-1], p!(i,j) == p!(i,j+1)]
  where m = length xss
        n = length (head xss)
        p = listArray ((1,1),(m,n)) (concat xss)

-- 3ª solución
-- ===========

numeroParesAdyacentesIguales3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales3 xss =
  length (concatMap tail (concatMap group (xss ++ transpose xss)))

-- 4ª solución
-- ===========

numeroParesAdyacentesIguales4 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales4 =
  length . (tail =<<) . (group =<<) . ((++) =<< transpose)

-- Comprobación de equivalencia
-- ============================

data Matriz = M [[Int]]
  deriving Show

-- Generador de matrices arbitrarias. Por ejemplo,
--    λ> generate matrizArbitraria
--    M [[-3,0],[8,-6],[-13,-13],[10,8],[14,29]]
--    λ> generate matrizArbitraria
--    M [[11,9,4,-25,-29,30,-18],[13,8,-2,-22,29,-3,-13]]
matrizArbitraria :: Gen Matriz
matrizArbitraria = do
  m <- chooseInt (1,10)
  n <- chooseInt (1,10)
  xss <- vectorOf m ((vectorOf n) arbitrary)
  return (M xss)

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz where
  arbitrary = matrizArbitraria

-- La propiedad es
prop_numeroParesAdyacentesIguales :: Matriz -> Bool
prop_numeroParesAdyacentesIguales (M xss) =
  all (== numeroParesAdyacentesIguales1 xss)
      [numeroParesAdyacentesIguales2 xss,
       numeroParesAdyacentesIguales3 xss,
       numeroParesAdyacentesIguales4 xss]

-- La comprobación es
--    λ> quickCheck prop_numeroParesAdyacentesIguales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroParesAdyacentesIguales1 (replicate (3*10^3) (replicate (10^3) 0))
--    5996000
--    (2.62 secs, 1,681,379,960 bytes)
--    λ> numeroParesAdyacentesIguales2 (replicate (3*10^3) (replicate (10^3) 0))
--    5996000
--    (5.51 secs, 4,751,249,472 bytes)
--    λ> numeroParesAdyacentesIguales3 (replicate (3*10^3) (replicate (10^3) 0))
--    5996000
--    (0.48 secs, 1,393,672,616 bytes)
--    λ> numeroParesAdyacentesIguales4 (replicate (3*10^3) (replicate (10^3) 0))
--    5996000
--    (0.38 secs, 1,393,560,848 bytes)
