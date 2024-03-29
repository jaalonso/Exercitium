-- MapPila.hs
-- Aplicación de una función a los elementos de una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las pilas](https://bit.ly/3GTToyK),
-- definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f p) es la pila formada con las imágenes por f de
-- los elementos de pila p, en el mismo orden. Por ejemplo,
--    λ> mapPila (+1) (apila 5 (apila 2 (apila 7 vacia)))
--    6 | 3 | 8
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MapPila where

import TAD.Pila (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (listaApila, pilaAlista)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

mapPila1 :: (a -> a) -> Pila a -> Pila a
mapPila1 f p
  | esVacia p = p
  | otherwise = apila (f cp) (mapPila1 f dp)
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

-- Se usarán las funciones listaApila y pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

mapPila2 :: (a -> a) -> Pila a -> Pila a
mapPila2 f p =
  listaApila (map f (pilaAlista p))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mapPila :: (Int -> Int) -> [Int] -> Bool
prop_mapPila f p =
  mapPila1 f q == mapPila2 f q
  where q = listaApila p

-- La comprobación es
--    λ> quickCheck' prop_mapPila
--    +++ OK, passed 100 tests.
