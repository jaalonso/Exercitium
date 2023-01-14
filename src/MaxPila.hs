-- MaxPila.hs
-- Máximo elemento de una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    maxPila :: Ord a => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo,
--    λ> maxPila (apila 3 (apila 5 (apila 1 vacia)))
--    5
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MaxPila where

import TAD.PilaConListas
import Test.QuickCheck

-- 1ª solución
-- ===========

maxPila1 :: Ord a => Pila a -> a
maxPila1 p
  | esVacia dp = cp
  | otherwise  = max cp (maxPila1 dp)
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

maxPila2 :: Ord a => Pila a -> a
maxPila2 =
  maximum . pilaAlista

-- (pilaAlista p) es la lista formada por los elementos de la
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
prop_maxPila :: Pila Int -> Property
prop_maxPila p =
  not (esVacia p) ==> maxPila1 p == maxPila2 p

-- La comprobación es
--    λ> quickCheck prop_maxPila
--    +++ OK, passed 100 tests; 17 discarded.
