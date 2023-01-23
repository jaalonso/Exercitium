-- LongitudCola.hs
-- TAD de las colas: Longitud de una cola.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    longitudCola :: Cola a -> Int
-- tal que (longitudCola c) es el número de elementos de la cola c. Por
-- ejemplo,
--    longitudCola (inserta 4 (inserta 2 (inserta 5 vacia))) == 3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module LongitudCola where

import TAD.Cola (Cola, vacia, inserta, resto, esVacia)
import Transformaciones_colas_listas (colaAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

longitudCola1 :: Cola a -> Int
longitudCola1 c
  | esVacia c = 0
  | otherwise = 1 + longitudCola1 rc
  where rc = resto c

-- 2ª solución
-- ===========

longitudCola2 :: Cola a -> Int
longitudCola2 = length . colaAlista

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_longitudCola :: Cola Int -> Bool
prop_longitudCola c =
  longitudCola1 c == longitudCola2 c

-- La comprobación es
--    λ> quickCheck prop_longitudCola
--    +++ OK, passed 100 tests.
