-- ExtiendeCola.hs
-- TAD de las colas: Extensión de colas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    extiendeCola :: Cola a -> Cola a -> Cola a
-- tal que (extiendeCola c1 c2) es la cola que resulta de poner los
-- elementos de la cola c2 a continuación de los de la cola de c1. Por
-- ejemplo,
--    λ> ej1 = inserta 3 (inserta 2 vacia)
--    λ> ej2 = inserta 5 (inserta 3 (inserta 4 vacia))
--    λ> ej1
--    2 | 3
--    λ> ej2
--    4 | 3 | 5
--    λ> extiendeCola ej1 ej2
--    2 | 3 | 4 | 3 | 5
--    λ> extiendeCola ej2 ej1
--    4 | 3 | 5 | 2 | 3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ExtiendeCola where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista, listaAcola)
import Test.QuickCheck

-- 1ª solución
-- ===========

extiendeCola :: Cola a -> Cola a -> Cola a
extiendeCola c1 c2
  | esVacia c2 = c1
  | otherwise  = extiendeCola (inserta pc2 c1) rq2
  where pc2 = primero c2
        rq2 = resto c2

-- 2ª solución
-- ===========

extiendeCola2 :: Cola a -> Cola a -> Cola a
extiendeCola2 c1 c2 =
  listaAcola (colaAlista c1 ++ colaAlista c2)

-- Las funciones colaAlista y listaAcola están definidas en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_extiendeCola :: Cola Int -> Cola Int -> Bool
prop_extiendeCola p c =
  extiendeCola p c == extiendeCola2 p c

-- La comprobación es
--    λ> quickCheck prop_extiendeCola
--    +++ OK, passed 100 tests.
