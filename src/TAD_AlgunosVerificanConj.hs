-- TAD_AlgunosVerificanConj.hs
-- TAD de los conjuntos: Algunos elementos verifican una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    algunos :: Ord a => (a -> Bool) -> Conj a -> Bool
-- tal que (algunos p c) se verifica si algún elemento de c verifica el
-- predicado p. Por ejemplo,
--    algunos even (inserta 4 (inserta 7 vacio))  ==  True
--    algunos even (inserta 3 (inserta 7 vacio))  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_AlgunosVerificanConj where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

algunos :: Ord a => (a -> Bool) -> Conj a -> Bool
algunos p c
  | esVacio c = False
  | otherwise = p mc || algunos p rc
  where mc = menor c
        rc = elimina mc c

-- 2ª solución
-- ===========

algunos2 :: Ord a => (a -> Bool) -> Conj a -> Bool
algunos2 p c = any p (conjuntoAlista c)

-- La función conjuntoAlista está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra
-- en https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_algunos :: (Int -> Bool) -> [Int] -> Bool
prop_algunos p xs =
  algunos p c == algunos2 p c
  where c = listaAconjunto xs

-- La comprobación es
--    λ> quickCheck' prop_algunos
--    +++ OK, passed 100 tests.
