-- TAD_Union_de_varios_conjuntos.hs
-- TAD de los conjuntos: Unión de varios conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    unionG:: Ord a => [Conj a] -> Conj a
-- tal (unionG cs) calcule la unión de la lista de conjuntos cd. Por
-- ejemplo,
--    λ> ej1 = inserta 3 (inserta 5 vacio)
--    λ> ej2 = inserta 5 (inserta 6 vacio)
--    λ> ej3 = inserta 3 (inserta 6 vacio)
--    λ> unionG [ej1, ej2, ej3]
--    {3, 5, 6}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Union_de_varios_conjuntos where

import TAD.Conjunto (Conj, vacio, inserta)
import TAD_Union_de_dos_conjuntos (union)
import Test.QuickCheck

-- 1ª solución
-- ===========

unionG :: Ord a => [Conj a] -> Conj a
unionG []          = vacio
unionG (c:cs) = c `union` unionG cs

-- 2ª solución
-- ===========

unionG2 :: Ord a => [Conj a] -> Conj a
unionG2 = foldr union vacio

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_unionG :: [Conj Int] -> Bool
prop_unionG cs =
  unionG cs == unionG2 cs

-- La comprobación es
--    λ> quickCheck prop_unionG
--    +++ OK, passed 100 tests.
