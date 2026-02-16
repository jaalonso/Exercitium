-- Diagonales_secundarias.hs
-- Diagonales secundarias de una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-Febrero-2015 (actualizado 16-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    diagonalesSecundarias :: Array (Int,Int) a -> [[a]]
-- tal que (diagonalesSecundarias p) es la lista de las diagonales
-- secundarias de p. Por ejemplo, para la matriz
--    1  2  3  4
--    5  6  7  8
--    9 10 11 12
-- la lista de sus diagonales secundarias es
--    [[1],[2,5],[3,6,9],[4,7,10],[8,11],[12]]
-- En Haskell,
--    λ> diagonalesSecundarias (listArray ((1,1),(3,4)) [1..12])
--    [[1],[2,5],[3,6,9],[4,7,10],[8,11],[12]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Diagonales_secundarias where

import Data.Array (Array, (!), bounds, listArray)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Matriz a = Array (Int,Int) a

-- 1ª solución: Basada en la propiedad de la suma de índices
-- =========================================================

diagonalesSecundarias1 :: Matriz a -> [[a]]
diagonalesSecundarias1 p =
  [[ p ! (i, s - i)
   | i <- [1..m],
     let j = s - i,
     j >= 1, j <= n ]
  | s <- [2..m+n] ]
  where
    (_, (m, n)) = bounds p

-- 2ª solución: Enfoque geométrico
-- ===============================

diagonalesSecundarias2 :: Matriz a -> [[a]]
diagonalesSecundarias2 p =
  [[p!ij1 | ij1 <- extension ij] | ij <- iniciales]
  where (_,(m,n)) = bounds p
        iniciales = [(1,j) | j <- [1..n]] ++ [(i,n) | i <- [2..m]]
        extension (i,j) = [(i+k,j-k) | k <- [0..min (j-1) (m-i)]]

-- 3ª solución: Evolución optimizada de la primera
-- ===============================================

diagonalesSecundarias3 :: Array (Int, Int) a -> [[a]]
diagonalesSecundarias3 p =
    [ [ p ! (i, s - i) | i <- [max 1 (s - n) .. min m (s - 1)] ]
    | s <- [2 .. m+n] ]
  where
    (_, (m, n)) = bounds p

-- 4ª solución: Enfoque estructural (estilo funcional puro)
-- ========================================================

diagonalesSecundarias4 :: Matriz a -> [[a]]
diagonalesSecundarias4 p = map catMaybes $ transpose filasDesplazadas
  where
    (_, (m, n)) = bounds p
    filas = [[Just (p ! (i, j)) | j <- [1..n]] | i <- [1..m]]
    filasDesplazadas = zipWith (\k fila -> replicate k Nothing ++ fila) [0..] filas

-- 5ª solución: Enfoque algebraico denso
-- =====================================

diagonalesSecundarias5 :: Matriz a -> [[a]]
diagonalesSecundarias5 v =
  [[v!(1+x+y, 1-y) | y <- [-min (n-1) x .. -max 0 (x-m+1)]]
  | x <- [0 .. m-1+n-1]]
  where (_, (m, n)) = bounds v

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Matriz Int -> [[Int]]) -> Spec
specG diagonalesSecundarias = do
  it "e1" $
    diagonalesSecundarias (listArray ((1,1),(3,4)) [1..12])
    `shouldBe` [[1],[2,5],[3,6,9],[4,7,10],[8,11],[12]]

spec :: Spec
spec = do
  describe "def. 1" $ specG diagonalesSecundarias1
  describe "def. 2" $ specG diagonalesSecundarias2
  describe "def. 3" $ specG diagonalesSecundarias3
  describe "def. 4" $ specG diagonalesSecundarias4
  describe "def. 5" $ specG diagonalesSecundarias5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

newtype Matriz2 = M (Array (Int,Int) Int)
  deriving Show

-- Generador de matrices arbitrarias. Por ejemplo,
--    λ> generate matrizArbitraria
--    M (array ((1,1),(3,4))
--             [((1,1),18),((1,2),6), ((1,3),-23),((1,4),-13),
--              ((2,1),-2),((2,2),22),((2,3),-25),((2,4),-5),
--              ((3,1),2), ((3,2),16),((3,3),-15),((3,4),7)])
matrizArbitraria :: Gen Matriz2
matrizArbitraria = do
  m  <- chooseInt (1,10)
  n  <- chooseInt (1,10)
  xs <- vectorOf (m*n) arbitrary
  return (M (listArray ((1,1),(m,n)) xs))

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz2 where
  arbitrary = matrizArbitraria

-- La propiedad es
prop_equivalencia :: Matriz2 -> Bool
prop_equivalencia (M p) =
  all (== diagonalesSecundarias1 p)
      [ diagonalesSecundarias2 p
      , diagonalesSecundarias3 p
      , diagonalesSecundarias4 p
      , diagonalesSecundarias5 p
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ejemplo = listArray ((1,1),(2000,2000)) [1..]
--    λ> sum (map sum (diagonalesSecundarias1 ejemplo))
--    8000002000000
--    (3.19 secs, 2,596,283,624 bytes)
--    λ> sum (map sum (diagonalesSecundarias2 ejemplo))
--    8000002000000
--    (3.37 secs, 2,660,139,488 bytes)
--    λ> sum (map sum (diagonalesSecundarias3 ejemplo))
--    8000002000000
--    (5.67 secs, 3,617,915,784 bytes)
--    λ> sum (map sum (diagonalesSecundarias4 ejemplo))
--    8000002000000
--    (2.46 secs, 2,083,307,544 bytes)
--    λ> sum (map sum (diagonalesSecundarias5 ejemplo))
--    8000002000000
--    (3.31 secs, 3,490,252,592 bytes)
