-- ContenidaPila.hs
-- Inclusión de pilas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las pilas](https://bit.ly/3GTToyK),
-- definir la función
--    contenidaPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si todos los elementos de
-- de la pila p1 son elementos de la pila p2. Por ejemplo,
--    λ> ej1 = apila 3 (apila 2 vacia)
--    λ> ej2 = apila 3 (apila 4 vacia)
--    λ> ej3 = apila 5 (apila 2 (apila 3 vacia))
--    λ> contenidaPila ej1 ej3
--    True
--    λ> contenidaPila ej2 ej3
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ContenidaPila where

import TAD.Pila (Pila, vacia, apila, esVacia, cima, desapila)
import PertenecePila (pertenecePila)
import Test.QuickCheck

-- 1ª solución
-- ===========

-- Se usará la función pertenecePila del ejercicio
-- "Pertenencia a una pila" que se encuentra en
-- https://bit.ly/3WdM9GC

contenidaPila1 :: Eq a => Pila a -> Pila a -> Bool
contenidaPila1 p1 p2
  | esVacia p1 = True
  | otherwise  = pertenecePila cp1 p2 && contenidaPila1 dp1 p2
  where cp1 = cima p1
        dp1 = desapila p1

-- 2ª solución
-- ===========

contenidaPila2 :: Eq a => Pila a -> Pila a -> Bool
contenidaPila2 p1 p2 =
  contenidaLista (pilaAlista p1) (pilaAlista p2)

contenidaLista :: Eq a => [a] -> [a] -> Bool
contenidaLista xs ys =
  all (`elem` ys) xs

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
prop_contenidaPila :: Pila Int -> Pila Int -> Bool
prop_contenidaPila p1 p2 =
  contenidaPila1 p1 p2 == contenidaPila2 p1 p2

-- La comprobación es
--    λ> quickCheck prop_contenidaPila
--    +++ OK, passed 100 tests.
