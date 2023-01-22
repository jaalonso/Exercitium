-- Transformaciones_pilas_listas.hs
-- Transformaciones entre pilas y listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las pilas](https://bit.ly/3GTToyK),
-- definir la función
--    listaApila :: [a] -> Pila a
--    pilaAlista :: Pila a -> [a]
-- tales que
-- + (listaApila xs) es la pila formada por los elementos de xs.
--   Por ejemplo,
--      λ> listaApila [3, 2, 5]
--      5 | 2 | 3
-- + (pilaAlista p) es la lista formada por los elementos de la
--   pila p. Por ejemplo,
--      λ> pilaAlista (apila 5 (apila 2 (apila 3 vacia)))
--      [3, 2, 5]
--
-- Comprobar con QuickCheck que ambas funciones son inversa; es decir,
--    pilaAlista (listaApila xs) = xs
--    listaApila (pilaAlista p)  = p
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Transformaciones_pilas_listas where

import TAD.Pila (Pila, vacia, apila, esVacia, cima, desapila)
import Test.QuickCheck

-- 1ª definición de listaApila
-- ===========================

listaApila :: [a] -> Pila a
listaApila ys = aux (reverse ys)
  where aux []     = vacia
        aux (x:xs) = apila x (aux xs)

-- 2ª definición de listaApila
-- ===========================

listaApila2 :: [a] -> Pila a
listaApila2 = aux . reverse
  where aux [] = vacia
        aux (x:xs) = apila x (aux xs)

-- 3ª definición de listaApila
-- ===========================

listaApila3 :: [a] -> Pila a
listaApila3 = aux . reverse
  where aux = foldr apila vacia

-- 4ª definición de listaApila
-- ===========================

listaApila4 :: [a] -> Pila a
listaApila4 xs = foldr apila vacia (reverse xs)

-- 5ª definición de listaApila
-- ===========================

listaApila5 :: [a] -> Pila a
listaApila5 = foldr apila vacia . reverse

-- Comprobación de equivalencia de las definiciones de listaApila
-- ==============================================================

-- La propiedad es
prop_listaApila :: [Int] -> Bool
prop_listaApila xs =
  all (== listaApila xs)
      [listaApila2 xs,
       listaApila3 xs,
       listaApila4 xs,
       listaApila5 xs]

-- La comprobación es
--    λ> quickCheck prop_listaApila
--    +++ OK, passed 100 tests.

-- 1ª definición de pilaAlista
-- ===========================

pilaAlista :: Pila a -> [a]
pilaAlista p
  | esVacia p = []
  | otherwise = pilaAlista dp ++ [cp]
  where cp = cima p
        dp = desapila p

-- 2ª definición de pilaAlista
-- ===========================

pilaAlista2 :: Pila a -> [a]
pilaAlista2 = reverse . aux
  where aux p | esVacia p = []
              | otherwise = cp : aux dp
          where cp = cima p
                dp = desapila p

-- Comprobación de equivalencia de las definiciones de pilaAlista
-- ==============================================================

-- La propiedad es
prop_pilaAlista :: Pila Int -> Bool
prop_pilaAlista p =
  pilaAlista p == pilaAlista2 p

-- La comprobación es
--    λ> quickCheck prop_pilaAlista
--    +++ OK, passed 100 tests.

-- Comprobación de las propiedades
-- ===============================

-- La primera propiedad es
prop_1_listaApila :: [Int] -> Bool
prop_1_listaApila xs =
  pilaAlista (listaApila xs) == xs

-- La comprobación es
--    λ> quickCheck prop_1_listaApila
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_2_listaApila :: Pila Int -> Bool
prop_2_listaApila p =
  listaApila (pilaAlista p) == p

-- La comprobación es
--    λ> quickCheck prop_2_listaApila
--    +++ OK, passed 100 tests.
