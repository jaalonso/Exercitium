-- TAD_Interseccion_de_dos_conjuntos.hs
-- TAD de los conjuntos: Intersección de dos conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    interseccion :: Ord a => Conj a -> Conj a -> Conj a
-- tal que (interseccion c1 c2) es la intersección de los conjuntos c1 y
-- c2. Por ejemplo,
--    λ> ej1 = inserta 3 (inserta 5 (inserta 2 vacio))
--    λ> ej2 = inserta 2 (inserta 4 (inserta 3 vacio))
--    λ> interseccion ej1 ej2
--    {2, 3}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Interseccion_de_dos_conjuntos where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, pertenece, esVacio)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Data.List (intersect)
import Test.QuickCheck

-- 1ª solución
-- ===========

interseccion :: Ord a => Conj a -> Conj a -> Conj a
interseccion c1 c2
  | esVacio c1       = vacio
  | pertenece mc1 c2 = inserta mc1 (interseccion rc1 c2)
  | otherwise        = interseccion rc1 c2
  where mc1 = menor c1
        rc1 = elimina mc1 c1

-- 2ª solución
-- ===========

interseccion2 :: Ord a => Conj a -> Conj a -> Conj a
interseccion2 c1 c2 =
  listaAconjunto [x | x <- conjuntoAlista c1, x `pertenece` c2]

-- Las funciones conjuntoAlista y listaAconjunto está definida en el
-- ejercicio Transformaciones entre conjuntos y listas" que se encuentra
-- en https://bit.ly/3RexzxH

-- 3ª solución
-- ===========

interseccion3 :: Ord a => Conj a -> Conj a -> Conj a
interseccion3 c1 c2 =
  listaAconjunto (conjuntoAlista c1 `intersect` conjuntoAlista c2)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_interseccion :: Conj Int -> Conj Int -> Bool
prop_interseccion c1 c2 =
  all (== interseccion c1 c2)
      [interseccion2 c1 c2,
       interseccion3 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.
