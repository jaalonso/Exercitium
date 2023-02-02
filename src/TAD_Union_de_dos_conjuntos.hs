-- TAD_Union_de_dos_conjuntos.hs
-- TAD de los conjuntos: Unión de dos conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    union :: Ord a => Conj a -> Conj a -> Conj a
-- tal (union c1 c2) es la unión de ambos conjuntos. Por ejemplo,
--    λ> ej1 = inserta 3 (inserta 5 vacio)
--    λ> ej2 = inserta 4 (inserta 3 vacio)
--    λ> union ej1 ej2
--    {3, 4, 5}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Union_de_dos_conjuntos where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, esVacio)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import qualified Data.List as L (union)
import Test.QuickCheck

-- 1ª solución
-- ===========

union :: Ord a => Conj a -> Conj a -> Conj a
union c1 c2
  | esVacio c1 = c2
  | otherwise  = inserta mc1 (rc1 `union` c2)
  where mc1 = menor c1
        rc1 = elimina mc1 c1

-- 2ª solución
-- ===========

union2 :: Ord a => Conj a -> Conj a -> Conj a
union2 c1 c2 =
  foldr inserta c2 (conjuntoAlista c1)

-- La función conjuntoAlista está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra en
-- https://bit.ly/3RexzxH

-- 3ª solución
-- ===========

union3 :: Ord a => Conj a -> Conj a -> Conj a
union3 c1 c2 =
  listaAconjunto (conjuntoAlista c1 `L.union` conjuntoAlista c2)

-- La función listaAconjunto está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra en
-- https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_union :: Conj Int -> Conj Int -> Bool
prop_union c1 c2 =
  all (== union c1 c2)
      [union2 c1 c2,
       union3 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_union
--    +++ OK, passed 100 tests.
