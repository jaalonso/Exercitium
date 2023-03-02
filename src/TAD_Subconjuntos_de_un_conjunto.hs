-- TAD_Subconjuntos_de_un_conjunto.hs
-- Subconjuntos de un conjunto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    potencia :: Ord a => Conj a -> Conj (Conj a)
-- tal que (potencia c) es el conjunto potencia de c; es decir, el
-- conjunto de todos los subconjuntos de c. Por ejemplo,
--    λ> potencia (inserta 1 (inserta 2 vacio))
--    {{}, {2}, {1}, {1, 2}}
--    λ> potencia (inserta 3 (inserta 1 (inserta 2 vacio)))
--    {{}, {3}, {2}, {2, 3}, {1}, {1, 3}, {1, 2}, {1, 2, 3}}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Subconjuntos_de_un_conjunto where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_mapC (mapC)
import TAD_Union_de_dos_conjuntos (union)
import TAD_subconjunto (subconjunto)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import Test.QuickCheck

-- 1ª solución
-- ===========

instance Ord a => Ord (Conj a) where
  c1 <= c2 = c1 `subconjunto` c2

-- La función subconjunto está definida en el ejercicio
-- "Reconocimiento de subconjuntos" que se encuentra en
-- http://bit.ly/2sqPtGs

potencia :: Ord a => Conj a -> Conj (Conj a)
potencia c
  | esVacio c = inserta vacio vacio
  | otherwise = mapC (inserta mc) pr `union` pr
  where mc = menor c
        rc = elimina mc c
        pr = potencia rc

-- La función mapC está definida en el ejercicio
-- "Aplicación de una función a los elementos de un conjunto" que se
-- encuentra en https://bit.ly/3Zbum5d

-- La función union está definida en el ejercicio
-- "Unión de dos conjuntos" que se encuentra en
-- https://bit.ly/3Y1jBl8

-- 2ª solución
-- ===========

potencia2 :: Ord a => Conj a -> Conj (Conj a)
potencia2 =
  listaAconjunto . map listaAconjunto . potenciaL . conjuntoAlista

potenciaL :: [a] -> [[a]]
potenciaL [] = [[]]
potenciaL (x:xs) = map (x:) pr ++ pr
  where pr = potenciaL xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_potencia :: Conj Int -> Bool
prop_potencia c =
  potencia c == potencia2 c

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_potencia
--    +++ OK, passed 100 tests.
