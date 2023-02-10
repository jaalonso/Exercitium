-- TAD_Particion_segun_un_numero.hs
-- TAD de los conjuntos: Partición según un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    divide :: (Ord a) => a-> Conj a -> (Conj a, Conj a)
-- tal que (divide x c) es el par formado por dos subconjuntos de c: el
-- de los elementos menores o iguales que x y el de los mayores que x.
-- Por ejemplo,
--    λ> divide 5 (inserta 7 (inserta 2 (inserta 8 vacio)))
--    ({2},{7, 8})
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Particion_segun_un_numero where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Particion_por_una_propiedad (particion)
import Test.QuickCheck

-- 1ª solución
-- ===========

divide :: Ord a => a-> Conj a -> (Conj a, Conj a)
divide x c
  | esVacio c = (vacio, vacio)
  | mc <= x   = (inserta mc c1, c2)
  | otherwise = (c1, inserta mc c2)
  where
    mc       = menor c
    rc       = elimina mc c
    (c1, c2) = divide x rc

-- 2ª solución
-- ===========

divide2 :: Ord a => a-> Conj a -> (Conj a, Conj a)
divide2 x = particion (<= x)

-- La función particion está definida en el ejercicio
-- "Partición de un conjunto según una propiedad" que se encuentra en
-- https://bit.ly/3YCOah5

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divide :: Int -> Conj Int -> Bool
prop_divide x c =
  divide x c == divide2 x c

-- La comprobación es
--    λ> quickCheck prop_divide
--    +++ OK, passed 100 tests.
