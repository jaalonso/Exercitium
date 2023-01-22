-- Transformaciones_colas_listas.hs
-- TAD de las colas: Transformaciones entre colas y listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir las funciones
--    listaAcola :: [a] -> Cola a
--    colaALista :: Cola a -> [a]
-- tales que
-- + (listaAcola xs) es la cola formada por los elementos de xs.
--   Por ejemplo,
--      λ> listaAcola [3, 2, 5]
--      3 | 2 | 5
-- + (colaALista c) es la lista formada por los elementos de la
--   lista c. Por ejemplo,
--      λ> colaAlista (inserta 5 (inserta 2 (inserta 3 vacia)))
--      [3, 2, 5]
--
-- Comprobar con QuickCheck que ambas funciones son inversa; es decir,
--    colaAlista (listaAcola xs) = xs
--    listaAcola (colaAlista c)  = c
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Transformaciones_colas_listas where

import TAD.Cola (Cola, vacia, inserta, esVacia, primero, resto)
import Test.QuickCheck

-- 1ª definición de listaAcola
-- ===========================

listaAcola :: [a] -> Cola a
listaAcola ys = aux (reverse ys)
  where aux []     = vacia
        aux (x:xs) = inserta x (aux xs)

-- 2ª definición de listaAcola
-- ===========================

listaAcola2 :: [a] -> Cola a
listaAcola2 = aux . reverse
  where aux [] = vacia
        aux (x:xs) = inserta x (aux xs)

-- 3ª definición de listaAcola
-- ===========================

listaAcola3 :: [a] -> Cola a
listaAcola3 = aux . reverse
  where aux = foldr inserta vacia

-- 4ª definición de listaAcola
-- ===========================

listaAcola4 :: [a] -> Cola a
listaAcola4 xs = foldr inserta vacia (reverse xs)

-- 5ª definición de listaAcola
-- ===========================

listaAcola5 :: [a] -> Cola a
listaAcola5 = foldr inserta vacia . reverse

-- Comprobación de equivalencia de las definiciones de listaAcola
-- ==============================================================

-- La propiedad es
prop_listaAcola :: [Int] -> Bool
prop_listaAcola xs =
  all (== listaAcola xs)
      [listaAcola2 xs,
       listaAcola3 xs,
       listaAcola4 xs,
       listaAcola5 xs]

-- La comprobación es
--    λ> quickCheck prop_listaAcola
--    +++ OK, passed 100 tests.

-- Definición de colaAlista
-- ========================

colaAlista :: Cola a -> [a]
colaAlista c
  | esVacia c = []
  | otherwise = pc : colaAlista rc
  where pc = primero c
        rc = resto c

-- Comprobación de las propiedades
-- ===============================

-- La primera propiedad es
prop_1_listaAcola :: [Int] -> Bool
prop_1_listaAcola xs =
  colaAlista (listaAcola xs) == xs

-- La comprobación es
--    λ> quickCheck prop_1_listaAcola
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_2_listaAcola :: Cola Int -> Bool
prop_2_listaAcola c =
  listaAcola (colaAlista c) == c

-- La comprobación es
--    λ> quickCheck prop_2_listaAcola
--    +++ OK, passed 100 tests.
