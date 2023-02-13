-- TodosVerificanConj.hs
-- TAD de los conjuntos: Todos los elementos verifican una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    todos :: Ord a => (a -> Bool) -> Conj a -> Bool
-- tal que (todos p c) se verifica si todos los elemsntos de c
-- verifican el predicado p.  Por ejemplo,
--    todos even (inserta 4 (inserta 6 vacio))  ==  True
--    todos even (inserta 4 (inserta 7 vacio))  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TodosVerificanConj where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

todos :: Ord a => (a -> Bool) -> Conj a -> Bool
todos p c
  | esVacio c = True
  | otherwise = p mc && todos p rc
  where mc = menor c
        rc = elimina mc c

-- 2ª solución
-- ===========

todos2 :: Ord a => (a -> Bool) -> Conj a -> Bool
todos2 p c = all p (conjuntoAlista c)

-- La función conjuntoAlista está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra
-- en https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_todos :: (Int -> Bool) -> [Int] -> Bool
prop_todos p xs =
  todos p c == todos2 p c
  where c = listaAconjunto xs

-- La comprobación es
--    λ> quickCheck' prop_todos
--    +++ OK, passed 100 tests.
