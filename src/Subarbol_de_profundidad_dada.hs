-- Subarbol_de_profundidad_dada.hs
-- Subárbol de profundidad dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La función take está definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
--
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir la función
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el subárbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--
-- Comprobar con QuickCheck que la profundidad de (takeArbol n x) es
-- menor o igual que n, para todo número natural n y todo árbol x.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Subarbol_de_profundidad_dada where

import Arboles_binarios (Arbol (..))
import Profundidad_de_un_arbol_binario (profundidad)
import Test.QuickCheck

takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H x)     = H x
takeArbol 0 (N x _ _) = H x
takeArbol n (N x i d) = N x (takeArbol (n-1) i) (takeArbol (n-1) d)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_takeArbol :: Int -> Arbol Int -> Property
prop_takeArbol n x =
  n >= 0 ==> profundidad (takeArbol n x) <= n

-- La comprobación es
--    λ> quickCheck prop_takeArbol
--    +++ OK, passed 100 tests.
