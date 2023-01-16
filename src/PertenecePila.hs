-- PertenecePila.hs
-- Pertenencia a una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    pertenecePila :: Eq a => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y es un elemento de la
-- pila p. Por ejemplo,
--    pertenecePila 2 (apila 5 (apila 2 (apila 3 vacia))) == True
--    pertenecePila 4 (apila 5 (apila 2 (apila 3 vacia))) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PertenecePila where

import TAD.PilaConListas (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (pilaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

pertenecePila :: Eq a => a -> Pila a -> Bool
pertenecePila x p
  | esVacia p  = False
  | otherwise  = x == cp || pertenecePila x dp
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

-- Se usará la función pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

pertenecePila2 :: Eq a => a -> Pila a -> Bool
pertenecePila2 x p =
  x `elem` pilaAlista p

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pertenecePila :: Int -> Pila Int -> Bool
prop_pertenecePila x p =
  pertenecePila x p == pertenecePila2 x p

-- La comprobación es
--    λ> quickCheck prop_pertenecePila
--    +++ OK, passed 100 tests.
