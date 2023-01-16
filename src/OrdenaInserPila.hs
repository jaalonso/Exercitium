-- OrdenaInserPila.hs
-- Ordenación de pilas por inserción.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    ordenaInserPila :: Ord a => Pila a -> Pila a
-- tal que (ordenaInserPila p) es la pila obtenida ordenando por
-- inserción los los elementos de la pila p. Por ejemplo,
--    λ> ordenaInserPila (apila 4 (apila 1 (apila 3 vacia)))
--    1 | 3 | 4
--
-- Comprobar con QuickCheck que la pila (ordenaInserPila p) está
-- ordenada.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OrdenaInserPila where

import TAD.PilaConListas (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (listaApila, pilaAlista)
import OrdenadaPila (ordenadaPila)
import Test.QuickCheck

-- 1ª solución
-- ===========

ordenaInserPila1 :: Ord a => Pila a -> Pila a
ordenaInserPila1 p
  | esVacia p = p
  | otherwise = insertaPila cp (ordenaInserPila1 dp)
  where cp = cima p
        dp = desapila p

insertaPila :: Ord a => a -> Pila a -> Pila a
insertaPila x p
  | esVacia p = apila x p
  | x < cp    = apila x p
  | otherwise = apila cp (insertaPila x dp)
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

ordenaInserPila2 :: Ord a => Pila a -> Pila a
ordenaInserPila2  =
  listaApila . reverse . ordenaInserLista . pilaAlista

ordenaInserLista :: Ord a => [a] -> [a]
ordenaInserLista []      = []
ordenaInserLista (x: xs) = insertaLista x (ordenaInserLista xs)

insertaLista :: Ord a => a -> [a] -> [a]
insertaLista x [] = [x]
insertaLista x (y:ys) | x < y = x : y : ys
                      | otherwise = y : insertaLista x ys

-- Se usarán las funciones listaApila y pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_ordenaInserPila :: Pila Int -> Bool
prop_ordenaInserPila p =
  ordenaInserPila1 p == ordenaInserPila2 p

-- La comprobación es
--    λ> quickCheck prop_ordenaInserPila
--    +++ OK, passed 100 tests.

-- Comprobación de la propiedad
-- ============================

-- Se usará la función ordenadaPila del ejercicio
-- "Reconocimiento de ordenación de pilas" que se encuentra en
-- https://bit.ly/3COqRbK

-- La propiedad es
prop_ordenadaOrdenaInserPila :: Pila Int -> Bool
prop_ordenadaOrdenaInserPila p =
  ordenadaPila (ordenaInserPila1 p)

-- La comprobación es
--    λ> quickCheck prop_ordenadaOrdenaInserPila
--    +++ OK, passed 100 tests.
