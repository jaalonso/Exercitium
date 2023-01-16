-- SubPila.hs
-- Reconocimiento de subpilas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    subPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2. Por
-- ejemplo,
--    λ> ej1 = apila 2 (apila 3 vacia)
--    λ> ej2 = apila 7 (apila 2 (apila 3 (apila 5 vacia)))
--    λ> ej3 = apila 2 (apila 7 (apila 3 (apila 5 vacia)))
--    λ> subPila ej1 ej2
--    True
--    λ> subPila ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module SubPila where

import TAD.PilaConListas (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (pilaAlista)
import PrefijoPila (prefijoPila)
import Data.List (isPrefixOf, tails)
import Test.QuickCheck

-- 1ª solución
-- ===========

-- Se usará la función PrefijoPila del ejercicio
-- "Reconocimiento de prefijos de pilas" que se encuentra en
-- https://bit.ly/3Xqu7lo

subPila1 :: Eq a => Pila a -> Pila a -> Bool
subPila1 p1 p2
    | esVacia p1 = True
    | esVacia p2 = False
    | cp1 == cp2 = prefijoPila dp1 dp2 || subPila1 p1 dp2
    | otherwise  = subPila1 p1 dp2
    where cp1 = cima p1
          dp1 = desapila p1
          cp2 = cima p2
          dp2 = desapila p2


-- 2ª solución
-- ===========

-- Se usará la función pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

subPila2 :: Eq a => Pila a -> Pila a -> Bool
subPila2 p1 p2 =
  sublista (pilaAlista p1) (pilaAlista p2)

-- (sublista xs ys) se verifica si xs es una sublista de ys. Por
-- ejemplo,
--    sublista [3,2] [5,3,2,7]  ==  True
--    sublista [3,2] [5,3,7,2]  ==  False
sublista :: Eq a => [a] -> [a] -> Bool
sublista xs ys =
  any (xs `isPrefixOf`) (tails ys)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subPila :: Pila Int -> Pila Int -> Bool
prop_subPila p1 p2 =
  subPila1 p1 p2 == subPila2 p1 p2

-- La comprobación es
--    λ> quickCheck prop_subPila
--    +++ OK, passed 100 tests.
