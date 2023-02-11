-- TAD_mapC.hs
-- TAD de los conjuntos: Aplicación de una función a los elementos de un conjunto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    mapC :: (Ord a, Ord b) => (a -> b) -> Conj a -> Conj b
-- tal que (map f c) es el conjunto formado por las imágenes de los
-- elementos de c, mediante f. Por ejemplo,
--    λ> mapC (*2) (inserta 3 (inserta 1 vacio))
--    {2, 6}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_mapC where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

mapC :: (Ord a, Ord b) => (a -> b) -> Conj a -> Conj b
mapC f c
  | esVacio c = vacio
  | otherwise = inserta (f mc) (mapC f rc)
  where mc = menor c
        rc = elimina mc c

-- 2ª solución
-- ===========

mapC2 :: (Ord a, Ord b) => (a -> b) -> Conj a -> Conj b
mapC2 f c = listaAconjunto (map f (conjuntoAlista c))

-- Las funciones conjuntoAlista y listaAconjunto está definida en el
-- ejercicio Transformaciones entre conjuntos y listas" que se encuentra
-- en https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mapC :: (Int -> Int) -> [Int] -> Bool
prop_mapC f xs =
  mapC f c == mapC2 f c
  where c = listaAconjunto xs

-- La comprobación es
--    λ> quickCheck' prop_mapC
--    +++ OK, passed 100 tests.
