-- UltimoCola.hs
-- TAD de las colas: Último elemento.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el último elemento de la cola c. Por
-- ejemplo:
--    ultimoCola (inserta 3 (inserta 5 (inserta 2 vacia))) == 3
--    ultimoCola (inserta 2 vacia)                         == 2
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module UltimoCola where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

ultimoCola :: Cola a -> a
ultimoCola c
  | esVacia c  = error "cola vacia"
  | esVacia rc = pc
  | otherwise  = ultimoCola rc
  where pc = primero c
        rc = resto c

-- 2ª solución
-- ===========

-- Se usarán la función colaAlista del ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

ultimoCola2 :: Cola a -> a
ultimoCola2 c
  | esVacia c  = error "cola vacia"
  | otherwise  = last (colaAlista c)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_ultimoCola :: Cola Int -> Property
prop_ultimoCola c =
  not (esVacia c) ==> ultimoCola c == ultimoCola2 c

-- La comprobación es
--    λ> quickCheck prop_ultimoCola
--    +++ OK, passed 100 tests; 16 discarded.
