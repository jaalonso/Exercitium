-- TAD_Conjuntos_disjuntos.hs
-- TAD de los conjuntos: Conjuntos disjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    disjuntos :: Ord a => Conj a -> Conj a -> Bool
-- tal que (disjuntos c1 c2) se verifica si los conjuntos c1 y c2 son
-- disjuntos. Por ejemplo,
--    λ> ej1 = inserta 2 (inserta 5 vacio)
--    λ> ej2 = inserta 4 (inserta 3 vacio)
--    λ> ej3 = inserta 5 (inserta 3 vacio)
--    λ> disjuntos ej1 ej2
--    True
--    λ> disjuntos ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Conjuntos_disjuntos where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina, pertenece)
import TAD_Interseccion_de_dos_conjuntos (interseccion)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

disjuntos :: Ord a => Conj a -> Conj a -> Bool
disjuntos c1 c2 = esVacio (interseccion c1 c2)

-- 2ª solución
-- ===========

disjuntos2 :: Ord a => Conj a -> Conj a -> Bool
disjuntos2 c1 c2
  | esVacio c1 = True
  | pertenece mc1 c2 = False
  | otherwise        = disjuntos2 rc1 c2
  where mc1 = menor c1
        rc1 = elimina mc1 c1

-- 3ª solución
-- ===========

disjuntos3 :: Ord a => Conj a -> Conj a -> Bool
disjuntos3 c1 c2 =
  all (`notElem` ys) xs
  where xs = conjuntoAlista c1
        ys = conjuntoAlista c2

-- La función conjuntoAlista está definida en el ejercicio
-- "Transformaciones entre conjuntos y listas" que se encuentra en
-- https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_disjuntos :: Conj Int -> Conj Int -> Bool
prop_disjuntos c1 c2 =
  all (== disjuntos c1 c2)
      [disjuntos2 c1 c2,
       disjuntos3 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_disjuntos
--    +++ OK, passed 100 tests.
