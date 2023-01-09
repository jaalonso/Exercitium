-- FiltraPila.hs
-- Filtrado de pilas según una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo de las listas (del un ejercicio anterior cuyo
-- código se encuentra en [PilaConListas.hs](https://bit.ly/3vL41xD))
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

import TAD.PilaConListas
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

filtraPila2 :: (a -> Bool) -> Pila a -> Pila a
filtraPila2 p q =
  listaApila (filter p (pilaAlista q))

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
prop_filtraPila :: (Int -> Bool) -> [Int] -> Bool
prop_filtraPila p xs =
  filtraPila1 p q == filtraPila2 p q
  where q = listaApila xs

-- La comprobación es
--    λ> quickCheck' prop_filtraPila
--    +++ OK, passed 100 tests.
