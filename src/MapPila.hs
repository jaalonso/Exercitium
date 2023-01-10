-- MapPila.hs
-- Aplicación de una función a los elementos de una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo de las listas (del un ejercicio anterior cuyo
-- código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f p) es la pila formada con las imágenes por f de
-- los elementos de pila p, en el mismo orden. Por ejemplo,
--    λ> mapPila1 (+1) (apila 5 (apila 2 (apila 7 vacia)))
--    6 | 3 | 8
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MapPila where

import TAD.PilaConListas
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

mapPila2 :: (a -> a) -> Pila a -> Pila a
mapPila2 f p =
  listaApila (map f (pilaAlista p))

-- (listaApila xs) es la pila formada por los elementos de xs.
-- Por ejemplo,
--    λ> listaApila [3, 2, 5]
--    5 | 2 | 3
listaApila :: [a] -> Pila a
listaApila = foldr apila vacia . reverse

-- (pilaALista p) es la lista formada por los elementos de la
-- lista p. Por ejemplo,
--    λ> pilaAlista (apila 5 (apila 2 (apila 3 vacia)))
--    [3, 2, 5]
pilaAlista :: Pila a -> [a]
pilaAlista = reverse . aux
  where aux p | esVacia p = []
              | otherwise = cp : aux dp
          where cp = cima p
                dp = desapila p

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
