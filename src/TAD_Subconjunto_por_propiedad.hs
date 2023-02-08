-- TAD_Subconjunto_por_propiedad.hs
-- TAD de los conjuntos: Subconjunto determinado por una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    filtra :: Ord a => (a -> Bool) -> Conj a -> Conj a
-- tal (filtra p c) es el conjunto de elementos de c que verifican el
-- predicado p. Por ejemplo,
--    λ> filtra even (inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio))))
--    {2, 4}
--    λ> filtra odd (inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio))))
--    {5, 7}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Subconjunto_por_propiedad where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

filtra :: Ord a => (a -> Bool) -> Conj a -> Conj a
filtra p c
  | esVacio c = vacio
  | p mc      = inserta mc (filtra p rc)
  | otherwise = filtra p rc
  where mc = menor c
        rc = elimina mc c

-- 2ª solución
-- ===========

filtra2 :: Ord a => (a -> Bool) -> Conj a -> Conj a
filtra2 p c =
  listaAconjunto (filter p (conjuntoAlista c))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_filtra :: (Int -> Bool) -> [Int] -> Bool
prop_filtra p xs =
  filtra p c == filtra2 p c
  where c = listaAconjunto xs

-- La comprobación es
--    λ> quickCheck' prop_filtra
--    +++ OK, passed 100 tests.
