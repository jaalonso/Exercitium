-- NubPila.hs
-- Eliminación de repeticiones en una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    nubPila :: Eq a => Pila a -> Pila a
-- tal que (nubPila p) es la pila con los elementos de p sin repeticiones.
-- Por ejemplo,
--    λ> nubPila (apila 3 (apila 1 (apila 3 (apila 5 vacia))))
--    1 | 3 | 5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module NubPila where

import TAD.PilaConListas (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (listaApila, pilaAlista)
import PertenecePila (pertenecePila)
import Data.List (nub)
import Test.QuickCheck

-- 1ª solución
-- ===========

-- Se usará la función pertenecePila del ejercicio
-- "Pertenencia a una pila" que se encuentra en
-- https://bit.ly/3WdM9GC

nubPila1 :: Eq a => Pila a -> Pila a
nubPila1 p
  | esVacia p           = vacia
  | pertenecePila cp dp = nubPila1 dp
  | otherwise           = apila cp (nubPila1 dp)
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

-- Se usarán las funciones listaApila y pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

nubPila2 :: Eq a => Pila a -> Pila a
nubPila2 =
  listaApila . nub . pilaAlista

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nubPila :: Pila Int -> Bool
prop_nubPila p =
  nubPila1 p == nubPila2 p

-- La comprobación es
--    λ> quickCheck prop_nubPila
--    +++ OK, passed 100 tests.
