-- MaxCola.hs
-- TAD de las colas: Máximo elemento de una cola.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    maxCola :: Ord a => Cola a -> a
-- tal que (maxCola c) sea el mayor de los elementos de la cola c. Por
-- ejemplo,
--    λ> maxCola (inserta 3 (inserta 5 (inserta 1 vacia)))
--    5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MaxCola where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import Transformaciones_colas_listas (colaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

maxCola1 :: Ord a => Cola a -> a
maxCola1 c
  | esVacia rc = pc
  | otherwise  = max pc (maxCola1 rc)
  where pc = primero c
        rc = resto c

-- 2ª solución
-- ===========

maxCola2 :: Ord a => Cola a -> a
maxCola2 =
  maximum . colaAlista

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_maxCola :: Cola Int -> Property
prop_maxCola c =
  not (esVacia c) ==> maxCola1 c == maxCola2 c

-- La comprobación es
--    λ> quickCheck prop_maxCola
--    +++ OK, passed 100 tests; 16 discarded.
