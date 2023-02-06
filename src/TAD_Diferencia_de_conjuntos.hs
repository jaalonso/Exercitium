-- TAD_Diferencia_de_conjuntos.hs
-- TAD de los conjuntos: Diferencia de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    diferencia :: Ord a => Conj a -> Conj a -> Conj a
-- tal que (diferencia c1 c2) es el conjunto de los elementos de c1 que
-- no son elementos de c2. Por ejemplo,
--    λ> ej1 = inserta 5 (inserta 3 (inserta 2 (inserta 7 vacio)))
--    λ> ej2 = inserta 7 (inserta 4 (inserta 3 vacio))
--    λ> diferencia ej1 ej2
--    {2, 5}
--    λ> diferencia ej2 ej1
--    {4}
--    λ> diferencia ej1 ej1
--    {}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Diferencia_de_conjuntos where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, pertenece, esVacio)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck

-- 1ª solución
-- ===========

diferencia :: Ord a => Conj a -> Conj a -> Conj a
diferencia c1 c2
  | esVacio c1       = vacio
  | pertenece mc1 c2 = diferencia rc1 c2
  | otherwise        = inserta mc1 (diferencia rc1 c2)
  where mc1 = menor c1
        rc1 = elimina mc1 c1

-- 2ª solución
-- ===========

diferencia2 :: Ord a => Conj a -> Conj a -> Conj a
diferencia2 c1 c2 =
  listaAconjunto [x | x <- conjuntoAlista c1, not (pertenece x c2)]

-- Las funciones conjuntoAlista y listaAconjunto está definida en el
-- ejercicio Transformaciones entre conjuntos y listas" que se encuentra
-- en https://bit.ly/3RexzxH

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_diferencia :: Conj Int -> Conj Int -> Bool
prop_diferencia c1 c2 =
  diferencia c1 c2 == diferencia2 c1 c2

-- La comprobación es
--    λ> quickCheck prop_diferencia
--    +++ OK, passed 100 tests.
