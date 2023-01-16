-- PrefijoPila.hs
-- Reconocimiento de prefijos de pilas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    prefijoPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    λ> ej1 = apila 4 (apila 2 vacia)
--    λ> ej2 = apila 4 (apila 2 (apila 5 vacia))
--    λ> ej3 = apila 5 (apila 4 (apila 2 vacia))
--    λ> prefijoPila ej1 ej2
--    True
--    λ> prefijoPila ej1 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PrefijoPila where

import TAD.PilaConListas (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (pilaAlista)
import Data.List (isSuffixOf)
import Test.QuickCheck

-- 1ª solución
-- ===========

prefijoPila :: Eq a => Pila a -> Pila a -> Bool
prefijoPila p1 p2
  | esVacia p1 = True
  | esVacia p2 = False
  | otherwise  = cp1 == cp2 && prefijoPila dp1 dp2
  where cp1 = cima p1
        dp1 = desapila p1
        cp2 = cima p2
        dp2 = desapila p2

-- 2ª solución
-- ===========

-- Se usará la función pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

prefijoPila2 :: Eq a => Pila a -> Pila a -> Bool
prefijoPila2 p1 p2 =
  pilaAlista p1 `isSuffixOf` pilaAlista p2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_prefijoPila :: Pila Int -> Pila Int -> Bool
prop_prefijoPila p1 p2 =
  prefijoPila p1 p2 == prefijoPila2 p1 p2

-- La comprobación es
--    λ> quickCheck prop_prefijoPila
--    +++ OK, passed 100 tests.
