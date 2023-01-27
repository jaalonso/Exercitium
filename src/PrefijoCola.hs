-- PrefijoCola.hs
-- Reconocimiento de prefijos de colas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    prefijoCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (prefijoCola c1 c2) se verifica si la cola c1 es justamente
-- un prefijo de la cola c2. Por ejemplo,
--    λ> ej1 = inserta 4 (inserta 2 vacia)
--    λ> ej2 = inserta 5 (inserta 4 (inserta 2 vacia))
--    λ> ej3 = inserta 5 (inserta 2 (inserta 4 vacia))
--    λ> prefijoCola ej1 ej2
--    True
--    λ> prefijoCola ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PrefijoCola where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import Transformaciones_colas_listas (colaAlista)
import Data.List (isPrefixOf)
import Test.QuickCheck

-- 1ª solución
-- ===========

prefijoCola :: Eq a => Cola a -> Cola a -> Bool
prefijoCola c1 c2
  | esVacia c1 = True
  | esVacia c2 = False
  | otherwise  = primero c1 == primero c2 &&
                 prefijoCola (resto c1) (resto c2)

-- 2ª solución
-- ===========

prefijoCola2 :: Eq a => Cola a -> Cola a -> Bool
prefijoCola2 c1 c2 =
  colaAlista c1 `isPrefixOf` colaAlista c2

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_prefijoCola :: Cola Int -> Cola Int -> Bool
prop_prefijoCola c1 c2 =
  prefijoCola c1 c2 == prefijoCola2 c1 c2

-- La comprobación es
--    λ> quickCheck prop_prefijoCola
--    +++ OK, passed 100 tests.
