-- Minimo_numero_de_cambios_para_igualar_una_lista.hs
-- Mínimo número de cambios para igualar una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-Febrero-2015 (actualizado 18-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    nMinimoCambios :: Ord a => [a] -> Int
-- tal que (nMinimoCambios xs) es el menor número de elementos de xs
-- que hay que cambiar para que todos sean iguales. Por ejemplo,
--    nMinimoCambios [3,5,3,7,9,6]     == 4
--    nMinimoCambios [3,5,3,7,3,3]     == 2
--    nMinimoCambios "Salamanca"        == 5
--    nMinimoCambios (4 : [1..3000000]) == 2999999
-- En el primer ejemplo, los elementos que hay que cambiar son 5, 7, 9 y
-- 6.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Minimo_numero_de_cambios_para_igualar_una_lista where

import Data.List (group, nub, sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Fuerza bruta
-- =========================

nMinimoCambios1 :: Ord a => [a] -> Int
nMinimoCambios1 [] = 0
nMinimoCambios1 xs =
  length xs - fst (maximum (frecuencias xs))

-- (frecuencias xs) es la lista de los pares de los elementos de xs y el
-- número de veces que ocurren en xs. Por ejemplo,
--    frecuencias [3,5,3,7,9,6]  ==  [(2,3),(1,5),(1,7),(1,9),(1,6)]
--    frecuencias [3,5,3,7,5,5]  ==  [(2,3),(3,5),(1,7)]
frecuencias :: Ord a => [a] -> [(Int,a)]
frecuencias xs = [(cuenta x xs,x) | x <- nub xs]

-- (cuenta x ys) es el número de veces que ocurre x en ys. Por ejemplo,
--    cuenta 3 [3,5,3,7,9,6]  ==  2
cuenta :: Ord a => a -> [a] -> Int
cuenta x ys = length [y | y <- ys, y == x]

-- 2ª solución: Ordenación y frecuencias con tuplas
-- ================================================

nMinimoCambios2 :: Ord a => [a] -> Int
nMinimoCambios2 [] = 0
nMinimoCambios2 xs =
  length xs - fst (maximum (frecuencias2 xs))

-- (frecuencias2 xs) es la lista de los pares de los elementos de xs y el
-- número de veces que ocurren en xs. Por ejemplo,
--    frecuencias2 [3,5,3,7,9,6]  ==  [(2,3),(1,5),(1,7),(1,9),(1,6)]
--    frecuencias2 [3,5,3,7,5,5]  ==  [(2,3),(3,5),(1,7)]
frecuencias2 :: Ord a  => [a] -> [(Int,a)]
frecuencias2 xs =
  [(1 + length ys, y) | (y:ys) <- group (sort xs)]

-- 3ª solución: Ordenación optimizada
-- ==================================

nMinimoCambios3 :: Ord a => [a] -> Int
nMinimoCambios3 [] = 0
nMinimoCambios3 xs = length xs - maximum ys
  where ys = map length (group (sort xs))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Int) -> Spec
specG nMinimoCambios = do
  it "e1" $
    nMinimoCambios [3,5,3,7,9,6] `shouldBe` 4
  it "e2" $
    nMinimoCambios [3,5,3,7,3,3] `shouldBe` 2

spec :: Spec
spec = do
  describe "def. 1" $ specG nMinimoCambios1
  describe "def. 2" $ specG nMinimoCambios2
  describe "def. 3" $ specG nMinimoCambios3

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
  all (== nMinimoCambios1 xs)
      [ nMinimoCambios2 xs
      , nMinimoCambios3 xs
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> nMinimoCambios1 (4 : [1..4000])
--    3999
--    (2.27 secs, 899,493,488 bytes)
--    λ> nMinimoCambios2 (4 : [1..4000])
--    3999
--    (0.03 secs, 2,918,056 bytes)
--    λ> nMinimoCambios3 (4 : [1..4000])
--    3999
--    (0.02 secs, 2,597,664 bytes)
--
--    λ> nMinimoCambios2 (4 : [1..3000000])
--    2999999
--    (2.36 secs, 1,728,616,336 bytes)
--    λ> nMinimoCambios3 (4 : [1..3000000])
--    2999999
--    (0.89 secs, 1,368,615,904 bytes)
