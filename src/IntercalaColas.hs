-- IntercalaColas.hs
-- TAD de las colas: Intercalado de dos colas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-febrero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el [tipo abstracto de datos de las colas](https://bit.ly/3QWTsRL),
-- definir la función
--    intercalaColas :: Cola a -> Cola a -> Cola a
-- tal que (intercalaColas c1 c2) es la cola formada por los elementos de
-- c1 y c2 colocados en una cola, de forma alternativa, empezando por
-- los elementos de c1. Por ejemplo,
--    λ> ej1 = inserta 3 (inserta 5 vacia)
--    λ> ej2 = inserta 0 (inserta 7 (inserta 4 (inserta 9 vacia)))
--    λ> intercalaColas ej1 ej2
--    5 | 9 | 3 | 4 | 7 | 0
--    λ> intercalaColas ej2 ej1
--    9 | 5 | 4 | 3 | 7 | 0
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module IntercalaColas where

import TAD.Cola (Cola, vacia, inserta, primero, resto, esVacia)
import Transformaciones_colas_listas (colaAlista, listaAcola)
import ExtiendeCola (extiendeCola)
import Test.QuickCheck

-- 1ª solución
-- ===========

intercalaColas :: Cola a -> Cola a -> Cola a
intercalaColas c1 c2
  | esVacia c1 = c2
  | esVacia c2 = c1
  | otherwise  = extiendeCola (inserta pc2 (inserta pc1 vacia))
                              (intercalaColas rc1 rc2)
  where pc1 = primero c1
        rc1 = resto c1
        pc2 = primero c2
        rc2 = resto c2

-- La función extiendeCola está definida en el ejercicio
-- "TAD de las colas: Extensión de colas" que se encuentra en
-- https://bit.ly/3XIJJ4m

-- 2ª solución
-- ===========

intercalaColas2 :: Cola a -> Cola a -> Cola a
intercalaColas2 c1 c2 = aux c1 c2 vacia
  where
    aux d1 d2 c
      | esVacia d1 = extiendeCola c d2
      | esVacia d2 = extiendeCola c d1
      | otherwise  = aux rd1 rd2 (inserta pd2 (inserta pd1 c))
      where pd1 = primero d1
            rd1 = resto d1
            pd2 = primero d2
            rd2 = resto d2

-- 3ª solución
-- ===========

intercalaColas3 :: Cola a -> Cola a -> Cola a
intercalaColas3 c1 c2 =
  listaAcola (intercalaListas (colaAlista c1) (colaAlista c2))

-- (intercalaListas xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
intercalaListas :: [a] -> [a] -> [a]
intercalaListas []     ys     = ys
intercalaListas xs     []     = xs
intercalaListas (x:xs) (y:ys) = x : y : intercalaListas xs ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_intercalaColas :: Cola Int -> Cola Int -> Bool
prop_intercalaColas c1 c2 =
  all (== intercalaColas c1 c2)
      [intercalaColas2 c1 c2,
       intercalaColas2 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_intercalaColas
--    +++ OK, passed 100 tests.
