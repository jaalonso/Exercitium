-- Minimo_producto_escalar.hs
-- Mínimo producto escalar.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 04-Febrero-2015 (actualizado 27-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El producto escalar de los vectores [a1,a2,...,an] y [b1,b2,..., bn]
-- es
--    a1 * b1 + a2 * b2 + ··· + an * bn.
--
-- Definir la función
--    menorProductoEscalar :: (Ord a, Num a) => [a] -> [a] -> a
-- tal que (menorProductoEscalar xs ys) es el mínimo de los productos
-- escalares de las permutaciones de xs y de las permutaciones de
-- ys. Por ejemplo,
--    menorProductoEscalar [3,2,5]  [1,4,6]    == 29
--    menorProductoEscalar [3,2,5]  [1,4,-6]   == -19
--    menorProductoEscalar [1..10^2] [1..10^2] == 171700
--    menorProductoEscalar [1..10^3] [1..10^3] == 167167000
--    menorProductoEscalar [1..10^4] [1..10^4] == 166716670000
--    menorProductoEscalar [1..10^5] [1..10^5] == 166671666700000
--    menorProductoEscalar [1..10^6] [1..10^6] == 166667166667000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Minimo_producto_escalar where

import Data.List (foldl', permutations, sort, sortBy, sortOn)
import Data.Ord (Down(..), comparing)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta basada en la definición (Muy ineficiente)
-- ===================================================================

menorProductoEscalar1 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar1 xs ys =
  minimum [sum (zipWith (*) pxs pys) | pxs <- permutations xs,
                                       pys <- permutations ys]

-- 2ª solución: Refinamiento de la fuerza bruta (Aún ineficiente)
-- ==============================================================

menorProductoEscalar2 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar2 xs ys =
  minimum [sum (zipWith (*) pxs ys) | pxs <- permutations xs]

-- 3ª solución: Enfoque algorítmico (Teorema de reordenamiento)
-- ============================================================

menorProductoEscalar3 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar3 xs ys =
  sum (zipWith (*) (sort xs) (reverse (sort ys)))

-- 4ª solución: Estilo declarativo (Listas por comprensión)
-- ========================================================

menorProductoEscalar4 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar4 xs ys =
  sum [x * y | (x, y) <- zip (sort xs) (sortOn Down ys)]

-- 5ª solución: Estilo funcional idiomático (zipWith + sortOn)
-- ===========================================================

menorProductoEscalar5 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar5 xs ys =
  sum (zipWith (*) (sort xs) (sortOn Down ys))

-- 6ª solución: Optimización de la ordenación (sortBy + comparing)
-- ===============================================================

menorProductoEscalar6 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar6 xs ys =
  sum (zipWith (*) (sort xs) (sortBy (comparing Down) ys))

-- 7ª solución: Optimización de memoria (Acumulación estricta con foldl')
-- ======================================================================

menorProductoEscalar7 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar7 xs ys =
  foldl' (+) 0 (zipWith (*) (sort xs) (sortOn Down ys))

-- 8ª solución: Fusión de bucles (zip + foldl' en un paso)
-- =======================================================

menorProductoEscalar8 :: (Ord a, Num a) => [a] -> [a] -> a
menorProductoEscalar8 xs ys =
  foldl' (+) 0 (zipWith (*) (sort xs) (sortBy (comparing Down) ys))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Integer] -> [Integer] -> Integer) -> Spec
specG menorProductoEscalar = do
  it "e1" $
    menorProductoEscalar [3,2,5]  [1,4,6] `shouldBe` 29
  it "e2" $
    menorProductoEscalar [3,2,5]  [1,4,-6] `shouldBe` -19
  it "e3" $
    menorProductoEscalar [] ([] :: [Integer]) `shouldBe` 0
  it "e4" $
    menorProductoEscalar [5] [3] `shouldBe` 15

spec :: Spec
spec = do
  describe "def. 1" $ specG menorProductoEscalar1
  describe "def. 2" $ specG menorProductoEscalar2
  describe "def. 3" $ specG menorProductoEscalar3
  describe "def. 4" $ specG menorProductoEscalar4
  describe "def. 5" $ specG menorProductoEscalar5
  describe "def. 6" $ specG menorProductoEscalar6
  describe "def. 7" $ specG menorProductoEscalar7
  describe "def. 8" $ specG menorProductoEscalar8

-- La verificación es
--    λ> verifica
--    32 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad para las 3 primeras soluciones
prop_menorProductoEscalar1 :: [Integer] -> [Integer] -> Bool
prop_menorProductoEscalar1 xs ys =
  all (== menorProductoEscalar1 xs' ys')
      [ menorProductoEscalar2 xs' ys'
      , menorProductoEscalar3 xs' ys'
      ]
  where n   = min (length xs) (length ys)
        xs' = take n xs
        ys' = take n ys

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_menorProductoEscalar1
--    +++ OK, passed 100 tests.

-- La propiedad para las restantes soluciones
prop_menorProductoEscalar2 :: [Integer] -> [Integer] -> Bool
prop_menorProductoEscalar2 xs ys =
  all (== menorProductoEscalar3 xs' ys')
      [ menorProductoEscalar4 xs' ys'
      , menorProductoEscalar5 xs' ys'
      , menorProductoEscalar6 xs' ys'
      , menorProductoEscalar7 xs' ys'
      , menorProductoEscalar8 xs' ys'
      ]
  where n   = min (length xs) (length ys)
        xs' = take n xs
        ys' = take n ys

-- La comprobación es
--    λ> quickCheck prop_menorProductoEscalar2
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> menorProductoEscalar1 [0..6] [0..6]
--    35
--    (21.58 secs, 40,304,707,600 bytes)
--    λ> menorProductoEscalar2 [0..6] [0..6]
--    35
--    (0.04 secs, 8,594,304 bytes)
--    λ> menorProductoEscalar2 [0..9] [0..9]
--    120
--    (3.70 secs, 7,242,359,848 bytes)
--    λ> menorProductoEscalar3 [0..9] [0..9]
--    120
--    (0.01 secs, 601,936 bytes)
--
--    λ> menorProductoEscalar3 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (2.17 secs, 1,630,167,040 bytes)
--    λ> menorProductoEscalar4 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (3.47 secs, 2,846,167,528 bytes)
--    λ> menorProductoEscalar5 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (2.56 secs, 2,430,167,328 bytes)
--    λ> menorProductoEscalar6 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (1.72 secs, 1,694,167,192 bytes)
--    λ> menorProductoEscalar7 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (1.84 secs, 2,430,167,352 bytes)
--    λ> menorProductoEscalar8 [0..4*10^6] [0..4*10^6]
--    10666666666666000000
--    (2.26 secs, 1,694,167,344 bytes)
