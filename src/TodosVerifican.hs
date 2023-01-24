-- TodosVerifican.hs
-- TAD de las colas: Todos los elementos verifican una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    todosVerifican :: (a -> Bool) -> Cola a -> Bool
-- tal que (todosVerifican p c) se verifica si todos los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    todosVerifican (>0) (inserta 3 (inserta 2 vacia))    == True
--    todosVerifican (>0) (inserta 3 (inserta (-2) vacia)) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TodosVerifican where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista, listaAcola)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

todosVerifican1 :: (a -> Bool) -> Cola a -> Bool
todosVerifican1 p c
  | esVacia c = True
  | otherwise = p pc && todosVerifican1 p rc
  where pc = primero c
        rc = resto c

-- 2ª solución
-- ===========

todosVerifican2 :: (a -> Bool) -> Cola a -> Bool
todosVerifican2 p c =
  all p (colaAlista c)

-- La función colaAlista está definida en el ejercicio
-- "Transformaciones entre colas y listas" que se encuentra en
-- https://bit.ly/3Xv0oIt

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_todosVerifican :: (Int -> Bool) -> [Int] -> Bool
prop_todosVerifican p xs =
  todosVerifican1 p c == todosVerifican2 p c
  where c = listaAcola xs

-- La comprobación es
--    λ> quickCheck' prop_todosVerifican
--    +++ OK, passed 100 tests.
