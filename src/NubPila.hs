-- NubPila.hs
-- Eliminación de repeticiones en una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    nubPila :: Eq a => Pila a -> Pila a
-- tal que (nubPila p) es la pila con los elementos de p sin repeticiones.
-- Por ejemplo,
--    λ> nubPila (apila 3 (apila 1 (apila 3 (apila 5 vacia))))
--    1 | 3 | 5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module NubPila where

import TAD.PilaConListas
import Data.List (nub)
import Test.QuickCheck

-- 1ª solución
-- ===========

nubPila1 :: Eq a => Pila a -> Pila a
nubPila1 p
  | esVacia p           = vacia
  | pertenecePila cp dp = nubPila1 dp
  | otherwise           = apila cp (nubPila1 dp)
  where cp = cima p
        dp = desapila p

-- (pertenecePila y p) se verifica si y es un elemento de la pila p. Por
-- ejemplo,
--    pertenecePila 2 (apila 5 (apila 2 (apila 3 vacia))) == True
--    pertenecePila 4 (apila 5 (apila 2 (apila 3 vacia))) == False
pertenecePila :: Eq a => a -> Pila a -> Bool
pertenecePila x p
  | esVacia p  = False
  | otherwise  = x == cp || pertenecePila x dp
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

nubPila2 :: Eq a => Pila a -> Pila a
nubPila2 =
  listaApila . nub . pilaAlista

-- (listaApila xs) es la pila formada por los elementos de xs.
-- Por ejemplo,
--    λ> listaApila [3, 2, 5]
--    5 | 2 | 3
listaApila :: [a] -> Pila a
listaApila ys = aux (reverse ys)
  where aux []     = vacia
        aux (x:xs) = apila x (aux xs)

-- (pilaALista p) es la lista formada por los elementos de la
-- lista p. Por ejemplo,
--    λ> pilaAlista (apila 5 (apila 2 (apila 3 vacia)))
--    [3, 2, 5]
pilaAlista :: Pila a -> [a]
pilaAlista p
  | esVacia p = []
  | otherwise = pilaAlista dp ++ [cp]
  where cp = cima p
        dp = desapila p

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nubPila :: Pila Int -> Bool
prop_nubPila p =
  nubPila1 p == nubPila2 p

-- La comprobación es
--    λ> quickCheck prop_nubPila
--    +++ OK, passed 100 tests.
