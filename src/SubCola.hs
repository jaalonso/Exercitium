-- SubCola.hs
-- TAD de las colas: Reconocimiento de subcolas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    subCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (subCola c1 c2) se verifica si c1 es una subcola de c2. Por
-- ejemplo,
--    λ> ej1 = inserta 2 (inserta 3 vacia)
--    λ> ej2 = inserta 7 (inserta 2 (inserta 3 (inserta 5 vacia)))
--    λ> ej3 = inserta 2 (inserta 7 (inserta 3 (inserta 5 vacia)))
--    λ> subCola ej1 ej2
--    True
--    λ> subCola ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module SubCola where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import Transformaciones_colas_listas (colaAlista)
import PrefijoCola (prefijoCola)
import Data.List (isPrefixOf, tails)
import Test.QuickCheck

-- 1ª solución
-- ===========

subCola1 :: Eq a => Cola a -> Cola a -> Bool
subCola1 c1 c2
    | esVacia c1 = True
    | esVacia c2 = False
    | pc1 == pc2 = prefijoCola rc1 rc2 || subCola1 c1 rc2
    | otherwise  = subCola1 c1 rc2
    where pc1 = primero c1
          rc1 = resto c1
          pc2 = primero c2
          rc2 = resto c2

-- La función PrefijoCola está definida en el ejercicio
-- "Reconocimiento de prefijos de colas" que se encuentra en
-- https://bit.ly/3HaK20x

-- 2ª solución
-- ===========

subCola2 :: Eq a => Cola a -> Cola a -> Bool
subCola2 c1 c2 =
  sublista (colaAlista c1) (colaAlista c2)

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- (sublista xs ys) se verifica si xs es una sublista de ys. Por
-- ejemplo,
--    sublista [3,2] [5,3,2,7]  ==  True
--    sublista [3,2] [5,3,7,2]  ==  False
sublista :: Eq a => [a] -> [a] -> Bool
sublista xs ys =
  any (xs `isPrefixOf`) (tails ys)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subCola :: Cola Int -> Cola Int -> Bool
prop_subCola c1 c2 =
  subCola1 c1 c2 == subCola2 c1 c2

-- La comprobación es
--    λ> quickCheck prop_subCola
--    +++ OK, passed 100 tests.
