-- TAD_subconjunto.hs
-- TAD de los conjuntos: Reconocimiento de subconjunto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 01-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    subconjunto :: Ord a => Conj a -> Conj a -> Bool
-- tal que (subconjunto c1 c2) se verifica si todos los elementos de c1
-- pertenecen a c2. Por ejemplo,
--    λ> ej1 = inserta 5 (inserta 2 vacio)
--    λ> ej2 = inserta 3 (inserta 2 (inserta 5 vacio))
--    λ> ej3 = inserta 3 (inserta 4 (inserta 5 vacio))
--    λ> subconjunto ej1 ej2
--    True
--    λ> subconjunto ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_subconjunto where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, pertenece, esVacio)
import Transformaciones_conjuntos_listas (conjuntoAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

subconjunto :: Ord a => Conj a -> Conj a -> Bool
subconjunto c1 c2
  | esVacio c1 = True
  | otherwise  =  pertenece mc1 c2 && subconjunto rc1 c2
  where mc1 = menor c1
        rc1 = elimina mc1 c1

-- 2ª solución
-- ===========

subconjunto2 :: Ord a => Conj a -> Conj a -> Bool
subconjunto2 c1 c2 =
  and [pertenece x c2 | x <- conjuntoAlista c1]

-- La función conjuntoAlista está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra en
-- https://bit.ly/3RexzxH

-- 3ª solución
-- ===========

subconjunto3 :: Ord a => Conj a -> Conj a -> Bool
subconjunto3 c1 c2 =
  all (`pertenece` c2) (conjuntoAlista c1)

-- 4ª solución
-- ===========

subconjunto4 :: Ord a => Conj a -> Conj a -> Bool
subconjunto4 c1 c2 =
  sublista (conjuntoAlista c1) (conjuntoAlista c2)

-- (sublista xs ys) se verifica si xs es una sublista de ys. Por
-- ejemplo,
--    sublista [5, 2] [3, 2, 5]  ==  True
--    sublista [5, 2] [3, 4, 5]  ==  False
sublista :: Ord a => [a] -> [a] -> Bool
sublista [] _      = True
sublista (x:xs) ys = elem x ys && sublista xs ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subconjunto :: Conj Int -> Conj Int -> Bool
prop_subconjunto c1 c2 =
  all (== subconjunto c1 c2)
      [subconjunto2 c1 c2,
       subconjunto3 c1 c2,
       subconjunto4 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_subconjunto
--    +++ OK, passed 100 tests.
