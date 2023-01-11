-- PertenecePila.hs
-- Pertenencia a una pila.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    pertenecePila :: Eq a => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y es un elemento de la
-- pila p. Por ejemplo,
--    pertenecePila 2 (apila 5 (apila 2 (apila 3 vacia))) == True
--    pertenecePila 4 (apila 5 (apila 2 (apila 3 vacia))) == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PertenecePila where

import TAD.PilaConListas
import Test.QuickCheck

-- 1ª solución
-- ===========

pertenecePila1 :: Eq a => a -> Pila a -> Bool
pertenecePila1 x p
  | esVacia p  = False
  | otherwise  = x == cp || pertenecePila1 x dp
  where cp = cima p
        dp = desapila p

-- 2ª solución
-- ===========

pertenecePila2 :: Eq a => a -> Pila a -> Bool
pertenecePila2 x p =
  x `elem` pilaAlista p

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
prop_pertenecePila :: Int -> Pila Int -> Bool
prop_pertenecePila x p =
  pertenecePila1 x p == pertenecePila2 x p

-- La comprobación es
--    λ> quickCheck prop_pertenecePila
--    +++ OK, passed 100 tests.
