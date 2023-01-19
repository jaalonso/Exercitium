-- MaxPila.hs
-- Máximo elemento de una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las pilas](https://bit.ly/3GTToyK),
-- definir la función
--    maxPila :: Ord a => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo,
--    λ> maxPila (apila 3 (apila 5 (apila 1 vacia)))
--    5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MaxPila where

import TAD.Pila (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (pilaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

maxPila1 :: Ord a => Pila a -> a
maxPila1 p
  | esVacia dp = cp
  | otherwise  = max cp (maxPila1 dp)
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

-- Se usará la función pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

maxPila2 :: Ord a => Pila a -> a
maxPila2 =
  maximum . pilaAlista

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_maxPila :: Pila Int -> Property
prop_maxPila p =
  not (esVacia p) ==> maxPila1 p == maxPila2 p

-- La comprobación es
--    λ> quickCheck prop_maxPila
--    +++ OK, passed 100 tests; 17 discarded.
