-- OrdenadaPila.hs
-- Reconocimiento de ordenación de pilas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    ordenadaPila :: Ord a => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila (apila 1 (apila 5 (apila 6 vacia))) == True
--    ordenadaPila (apila 1 (apila 0 (apila 6 vacia))) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OrdenadaPila where

import TAD.PilaConListas
import Test.QuickCheck

-- 1ª solución
-- ===========

ordenadaPila1 :: Ord a => Pila a -> Bool
ordenadaPila1 p
  | esVacia p  = True
  | esVacia dp = True
  | otherwise  = cp <= cdp && ordenadaPila1 dp
  where cp  = cima p
        dp  = desapila p
        cdp = cima dp

-- 2ª solución
-- ===========

ordenadaPila2 :: Ord a => Pila a -> Bool
ordenadaPila2 =
  ordenadaLista . reverse . pilaAlista

-- (ordenadaLista xs) se verifica si la lista xs está ordenada de menor
-- a mayor. Por ejemplo,
ordenadaLista :: Ord a => [a] -> Bool
ordenadaLista xs =
  and [x <= y | (x,y) <- zip xs (tail xs)]

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
prop_ordenadaPila :: Pila Int -> Bool
prop_ordenadaPila p =
  ordenadaPila1 p == ordenadaPila2 p

-- La comprobación es
--    λ> quickCheck prop_ordenadaPila
--    +++ OK, passed 100 tests.
