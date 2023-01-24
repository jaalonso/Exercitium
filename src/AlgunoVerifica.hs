-- AlgunoVerifica.hs
-- TAD de las colas: Alguno los elementos verifican una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    algunoVerifica :: (a -> Bool) -> Cola a -> Bool
-- tal que (algunoVerifica p c) se verifica si alguno de los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    algunoVerifica (< 0) (inserta 3 (inserta (-2) vacia)) == True
--    algunoVerifica (< 0) (inserta 3 (inserta 2 vacia))    == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module AlgunoVerifica where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista, listaAcola)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

algunoVerifica1 :: (a -> Bool) -> Cola a -> Bool
algunoVerifica1 p c
  | esVacia c = False
  | otherwise = p pc || algunoVerifica1 p rc
  where pc = primero c
        rc = resto c

-- 2ª solución
-- ===========

algunoVerifica2 :: (a -> Bool) -> Cola a -> Bool
algunoVerifica2 p c =
  any p (colaAlista c)

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_algunoVerifica :: (Int -> Bool) -> [Int] -> Bool
prop_algunoVerifica p xs =
  algunoVerifica1 p c == algunoVerifica2 p c
  where c = listaAcola xs

-- La comprobación es
--    λ> quickCheck' prop_algunoVerifica
--    +++ OK, passed 100 tests.
