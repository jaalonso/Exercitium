-- TAD_Interseccion_de_varios_conjuntos.hs
-- TAD de los conjuntos: Intersección de varios conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    interseccionG:: Ord a => [Conj a] -> Conj a
-- tal que (interseccionG cs) es la intersección de la lista de
-- conjuntos cs. Por ejemplo,
--    λ> ej1 = inserta 2 (inserta 3 (inserta 5 vacio))
--    λ> ej2 = inserta 5 (inserta 2 (inserta 7 vacio))
--    λ> ej3 = inserta 3 (inserta 2 (inserta 5 vacio))
--    λ> interseccionG [ej1, ej2, ej3]
--    {2, 5}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Interseccion_de_varios_conjuntos where

import TAD.Conjunto (Conj, vacio, inserta)
import TAD_Interseccion_de_dos_conjuntos (interseccion)
import Test.QuickCheck

-- 1ª solución
-- ===========

interseccionG :: Ord a => [Conj a] -> Conj a
interseccionG [c]      = c
interseccionG (cs:css) = interseccion cs (interseccionG css)

-- La función interseccion está definida en el ejercicio
-- "Intersección de dos conjuntos" que se encuentra en
-- https://bit.ly/3jDL9xZ

-- 2ª solución
-- ===========

interseccionG2 :: Ord a => [Conj a] -> Conj a
interseccionG2 = foldr1 interseccion

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_interseccionG :: NonEmptyList (Conj Int) -> Bool
prop_interseccionG (NonEmpty cs) =
  interseccionG cs == interseccionG2 cs

-- La comprobación es
--    λ> quickCheck prop_interseccionG1
--    +++ OK, passed 100 tests.
