-- TAD_Numero_de_elementos_de_un_conjunto.hs
-- TAD de los conjuntos: Número de elementos de un conjunto
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de los conjuntos](https://bit.ly/3HbB7fo)
-- definir la función
--    cardinal :: Conj a -> Int
-- tal que (cardinal c) es el número de elementos del conjunto c. Por
-- ejemplo,
--    cardinal (inserta 4 (inserta 5 vacio))             == 2
--    cardinal (inserta 4 (inserta 5 (inserta 4 vacio))) == 2
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Numero_de_elementos_de_un_conjunto where

import TAD.Conjunto (Conj, vacio, inserta, menor, elimina, esVacio)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista)
import Test.QuickCheck

-- 1ª solución
-- ===========

cardinal :: Ord a => Conj a -> Int
cardinal c
  | esVacio c = 0
  | otherwise = 1 + cardinal (elimina (menor c) c)

-- 2ª solución
-- ===========

cardinal2 :: Ord a => Conj a -> Int
cardinal2 = length . conjuntoAlista

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_cardinal :: Conj Int -> Bool
prop_cardinal c =
  cardinal c == cardinal2 c

-- La comprobación es
--    λ> quickCheck prop_cardinal
--    +++ OK, passed 100 tests.
