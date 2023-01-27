-- PerteneceCola.hs
-- TAD de las colas: Pertenencia a una cola.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
--    perteneceCola :: Eq a => a -> Cola a -> Bool
-- tal que (perteneceCola x c) se verifica si x es un elemento de la
-- cola c. Por ejemplo,
--    perteneceCola 2 (inserta 5 (inserta 2 (inserta 3 vacia))) == True
--    perteneceCola 4 (inserta 5 (inserta 2 (inserta 3 vacia))) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PerteneceCola where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista, listaAcola)
import Test.QuickCheck

-- 1ª solución
-- ===========

perteneceCola :: Eq a => a -> Cola a -> Bool
perteneceCola x c
  | esVacia c = False
  | otherwise = x == primero(c) || perteneceCola x (resto c)

-- 2ª solución
-- ===========

perteneceCola2 :: Eq a => a -> Cola a -> Bool
perteneceCola2 x c =
  x `elem` colaAlista c

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_perteneceCola :: Int -> Cola Int -> Bool
prop_perteneceCola x p =
  perteneceCola x p == perteneceCola2 x p

-- La comprobación es
--    λ> quickCheck prop_perteneceCola
--    +++ OK, passed 100 tests.
