-- FiltraPila.hs
-- Filtrado de pilas según una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo de dato de las pilas](https://bit.ly/3GTToyK)
-- (cuyo código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
-- definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p q) es la pila obtenida con los elementos de
-- pila q que verifican el predicado p, en el mismo orden. Por ejemplo,
--    λ> ejPila = apila 6 (apila 3 (apila 1 (apila 4 vacia)))
--    λ> ejPila
--    6 | 3 | 1 | 4
--    λ> filtraPila even ejPila
--    6 | 4
--    λ> filtraPila odd ejPila
--    3 | 1
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module FiltraPila where

import TAD.Pila (Pila, vacia, apila, esVacia, cima, desapila)
import Transformaciones_pilas_listas (listaApila, pilaAlista)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

filtraPila1 :: (a -> Bool) -> Pila a -> Pila a
filtraPila1 p q
  | esVacia q = vacia
  | p cq      = apila cq (filtraPila1 p dq)
  | otherwise = filtraPila1 p dq
  where cq = cima q
        dq = desapila q

-- 2ª solución
-- ===========

-- Se usarán las funciones listaApila y pilaAlista del ejercicio
-- "Transformaciones entre pilas y listas" que se encuentra en
-- https://bit.ly/3ZHewQ8

filtraPila2 :: (a -> Bool) -> Pila a -> Pila a
filtraPila2 p q =
  listaApila (filter p (pilaAlista q))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_filtraPila :: (Int -> Bool) -> [Int] -> Bool
prop_filtraPila p xs =
  filtraPila1 p q == filtraPila2 p q
  where q = listaApila xs

-- La comprobación es
--    λ> quickCheck' prop_filtraPila
--    +++ OK, passed 100 tests.
