-- OrdenadaCola.hs
-- Reconocimiento de ordenación de colas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    ordenadaCola :: Ord a => Cola a -> Bool
-- tal que (ordenadaCola c) se verifica si los elementos de la cola c
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaCola (inserta 6 (inserta 5 (inserta 1 vacia))) == True
--    ordenadaCola (inserta 1 (inserta 0 (inserta 6 vacia))) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OrdenadaCola where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import Transformaciones_colas_listas (colaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

ordenadaCola :: Ord a => Cola a -> Bool
ordenadaCola c
  | esVacia c  = True
  | esVacia rc = True
  | otherwise  = pc <= prc && ordenadaCola rc
  where pc  = primero c
        rc  = resto c
        prc = primero rc

-- 2ª solución
-- ===========

ordenadaCola2 :: Ord a => Cola a -> Bool
ordenadaCola2 =
  ordenadaLista . colaAlista

-- (ordenadaLista xs) se verifica si la lista xs está ordenada de menor
-- a mayor. Por ejemplo,
ordenadaLista :: Ord a => [a] -> Bool
ordenadaLista xs =
  and [x <= y | (x,y) <- zip xs (tail xs)]

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_ordenadaCola :: Cola Int -> Bool
prop_ordenadaCola c =
  ordenadaCola c == ordenadaCola2 c

-- La comprobación es
--    λ> quickCheck prop_ordenadaCola
--    +++ OK, passed 100 tests.
