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

import TAD.PilaConListas
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
prop_ordenaInserPila :: Pila Int -> Bool
prop_ordenaInserPila p =
  ordenaInserPila1 p == ordenaInserPila2 p

-- La comprobación es



-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_ordenadaOrdenaInserPila :: Pila Int -> Bool
prop_ordenadaOrdenaInserPila p =
  ordenadaPila (ordenaInserPila1 p)

ordenadaPila :: Ord a => Pila a -> Bool
ordenadaPila p
  | esVacia p  = True
  | esVacia dp = True
  | otherwise  = cp <= cdp && ordenadaPila dp
  where cp  = cima p
        dp  = desapila p
        cdp = cima dp

-- La comprobación es
--    λ> quickCheck prop_ordenadaOrdenaInserPila
--    +++ OK, passed 100 tests.
