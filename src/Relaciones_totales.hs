-- Relaciones_totales.hs
-- Relaciones totales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las relaciones binarias](https://bit.ly/3IVVqOT),
-- definir la función
--    total :: Eq a => Rel a -> Bool
-- tal que (total r) se verifica si la relación r es total; es decir, si
-- para cualquier par x, y de elementos del universo de r, se tiene que
-- x está relacionado con y o y está relacionado con x. Por ejemplo,
--    total (R ([1,3],[(1,1),(3,1),(3,3)]))  ==  True
--    total (R ([1,3],[(1,1),(3,1)]))        ==  False
--    total (R ([1,3],[(1,1),(3,3)]))        ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Relaciones_totales where

import Relaciones_binarias (Rel(R))
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

total :: Eq a => Rel a -> Bool
total (R (u,g)) =
  and [(x,y) `elem` g || (y,x) `elem` g | x <- u, y <- u]

-- 1ª solución
-- ===========

total2 :: Eq a => Rel a -> Bool
total2 (R (u,g)) =
  all (relacionados g) (producto u u)

producto :: [a] -> [a] -> [(a,a)]
producto xs ys =
  [(x,y) | x <- xs, y <- ys]

relacionados :: Eq a => [(a,a)] -> (a,a) -> Bool
relacionados g (x,y) =
  (x,y) `elem` g || (y,x) `elem` g

-- 3ª solución
-- ===========

total3 :: Eq a => Rel a -> Bool
total3 (R (u,g)) = aux1 u
  where aux1 []       = True
        aux1 (x:xs)   = aux2 x u && aux1 xs
        aux2 _ []     = True
        aux2 x (y:ys) = relacionados g (x,y) && aux2 x ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_total :: Rel Int -> Bool
prop_total r =
  all (== total r)
      [total2 r,
       total3 r]

-- La comprobación es
--    λ> quickCheck prop_total
--    +++ OK, passed 100 tests.
