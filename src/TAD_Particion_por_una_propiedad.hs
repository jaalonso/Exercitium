-- TAD_Particion_por_una_propiedad.hs
-- TAD de los conjuntos: Partición de un conjunto según una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    particion :: Ord a => (a -> Bool) -> Conj a -> (Conj a, Conj a)
-- tal que (particion c) es el par formado por dos conjuntos: el de sus
-- elementos que verifican p y el de los elementos que no lo
-- verifica. Por ejemplo,
--    λ> ej = inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio)))
--    λ> particion even ej
--    ({2, 4},{5, 7})
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Particion_por_una_propiedad where

import TAD.Conjunto (Conj, vacio, inserta)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import TAD_Subconjunto_por_propiedad (filtra)
import Data.List (partition)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

particion :: Ord a => (a -> Bool) -> Conj a -> (Conj a, Conj a)
particion p c = (filtra p c, filtra (not . p) c)

-- La función filtra está definida en el ejercicio
-- "Subconjunto determinado por una propiedad" que se encuentra en
-- https://bit.ly/3lplFoV

-- 2ª solución
-- ===========

particion2 :: Ord a => (a -> Bool) -> Conj a -> (Conj a, Conj a)
particion2 p c = (listaAconjunto xs, listaAconjunto ys)
  where
    (xs, ys) = partition p (conjuntoAlista c)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_particion :: (Int -> Bool) -> [Int] -> Bool
prop_particion p xs =
  particion p c == particion2 p c
  where c = listaAconjunto xs

-- La comprobación es
--    λ> quickCheck' prop_particion
--    +++ OK, passed 100 tests.
