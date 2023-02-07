-- TAD_Diferencia_simetrica.hs
-- TAD de los conjuntos: Diferencia simétrica.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    diferenciaSimetrica :: Ord a => Conj a -> Conj a -> Conj a
-- tal que (diferenciaSimetrica c1 c2) es la diferencia simétrica de los
-- conjuntos c1 y c2. Por ejemplo,
--    λ> ej1 = inserta 5 (inserta 3 (inserta 2 (inserta 7 vacio)))
--    λ> ej2 = inserta 7 (inserta 4 (inserta 3 vacio))
--    λ> diferenciaSimetrica ej1 ej2
--    {2, 4, 5}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Diferencia_simetrica where

import TAD.Conjunto (Conj, vacio, inserta)
import TAD_Diferencia_de_conjuntos (diferencia)
import TAD_Interseccion_de_dos_conjuntos (interseccion)
import TAD_Union_de_dos_conjuntos (union)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)

import Test.QuickCheck

-- 1ª solución
-- ===========

diferenciaSimetrica :: Ord a => Conj a -> Conj a -> Conj a
diferenciaSimetrica c1 c2 =
  diferencia (union c1 c2) (interseccion c1 c2)

-- 2ª solución
-- ===========

diferenciaSimetrica2 :: Ord a => Conj a -> Conj a -> Conj a
diferenciaSimetrica2 c1 c2 =
  listaAconjunto ([x | x <- xs, x `notElem` ys] ++
                  [y | y <- ys, y `notElem` xs])
  where xs = conjuntoAlista c1
        ys = conjuntoAlista c2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_diferenciaSimetrica :: Conj Int -> Conj Int -> Bool
prop_diferenciaSimetrica c1 c2 =
  diferenciaSimetrica c1 c2 == diferenciaSimetrica2 c2 c1

-- La comprobación es
--    λ> quickCheck prop_diferenciaSimetrica
--    +++ OK, passed 100 tests.
