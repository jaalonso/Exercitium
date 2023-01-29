-- Transformaciones_conjuntos_listas.hs
-- TAD de los conjuntos: Transformaciones entre conjuntos y listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir las funciones
--    listaAconjunto :: [a] -> Conj a
--    conjuntoAlista :: Conj a -> [a]
-- tales que
-- + (listaAconjunto xs) es el conjunto formado por los elementos de xs.
--   Por ejemplo,
--      λ> listaAconjunto [3, 2, 5]
--      {2, 3, 5}
-- + (conjuntoAlista c) es la lista formada por los elementos del
--   conjunto c. Por ejemplo,
--      λ> conjuntoAlista (inserta 5 (inserta 2 (inserta 3 vacio)))
--      [2,3,5]
--
-- Comprobar con QuickCheck que ambas funciones son inversa; es decir,
--    conjuntoAlista (listaAconjunto xs) = sort (nub xs)
--    listaAconjunto (conjuntoAlista c)  = c
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Transformaciones_conjuntos_listas where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, pertenece, esVacio)
import Data.List (sort, nub)
import Test.QuickCheck

-- 1ª definición de listaAconjunto
-- ===============================

listaAconjunto :: Ord a => [a] -> Conj a
listaAconjunto []     = vacio
listaAconjunto (x:xs) = inserta x (listaAconjunto xs)

-- 2ª definición de listaAconjunto
-- ===============================

listaAconjunto2 :: Ord a => [a] -> Conj a
listaAconjunto2 = foldr inserta vacio

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_listaAconjunto :: [Int] -> Bool
prop_listaAconjunto xs =
  listaAconjunto xs == listaAconjunto2 xs

-- La comprobación es
--    λ> quickCheck prop_listaAconjunto
--    +++ OK, passed 100 tests.

-- Definición de conjuntoAlista
-- ============================

conjuntoAlista :: Ord a => Conj a -> [a]
conjuntoAlista c
  | esVacio c = []
  | otherwise = mc : conjuntoAlista rc
  where mc = menor c
        rc = elimina mc c

-- Comprobación de las propiedades
-- ===============================

-- La primera propiedad es
prop_1_listaAconjunto :: [Int] -> Bool
prop_1_listaAconjunto xs =
  conjuntoAlista (listaAconjunto xs) == sort (nub xs)

-- La comprobación es
--    λ> quickCheck prop_1_listaAconjunto
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_2_listaAconjunto :: Conj Int -> Bool
prop_2_listaAconjunto c =
  listaAconjunto (conjuntoAlista c) == c

-- La comprobación es
--    λ> quickCheck prop_2_listaAconjunto
--    +++ OK, passed 100 tests.
