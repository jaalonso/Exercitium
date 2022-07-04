-- Composicion_de_relaciones_binarias.hs
-- Composición de relaciones binarias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las relaciones binarias en un conjunto A se pueden representar
-- mediante conjuntos de pares de elementos de A. Por ejemplo, la
-- relación de divisibilidad en el conjunto {1,2,3,6} se representa por
--    [(1,1),(1,2),(1,3),(1,6),(2,2),(2,6),(3,3),(3,6),(6,6)]
--
-- La composición de dos relaciones binarias R y S en el conjunto A es
-- la relación binaria formada por los pares (x,y) para los que existe
-- un z tal que (x,z) ∈ R y (z,y) ∈ S.
-- 
-- Definir la función
--    composicion :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
-- tal que (composicion r s) es la composición de las relaciones
-- binarias r y s. Por ejemplo,
--    λ> composicion [(1,2)] [(2,3),(2,4)]
--    [(1,3),(1,4)]
--    λ> composicion [(1,2),(5,2)] [(2,3),(2,4)]
--    [(1,3),(1,4),(5,3),(5,4)]
--    λ> composicion [(1,2),(1,4),(1,5)] [(2,3),(4,3)]
--    [(1,3)]
--
-- Nota: Se supone que las relaciones binarias son listas sin
-- elementos repetidos. 
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE MonadComprehensions #-}

module Composicion_de_relaciones_binarias where

import Data.List (nub, sort)
import qualified  Data.Set.Monad as S (Set, fromList, toList)
import Test.QuickCheck

-- 1ª solución
-- ===========

composicion1 :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion1 r s =
  nub [(x,y) | (x,u) <- r, (v,y) <- s, u == v] 

-- 2ª solución
-- ===========

composicion2 :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion2 r s =
  S.toList (composicionS (S.fromList r) (S.fromList s))

composicionS :: Ord a => S.Set (a,a) -> S.Set (a,a) -> S.Set (a,a)
composicionS r s =  
  [(x,y) | (x,u) <- r, (v,y) <- s, u == v] 

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_composicion :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_composicion r s =
  sort (composicion1 r' s') == composicion2 r' s'
  where r' = nub r
        s' = nub s

-- La comprobación es
--    λ> quickCheck prop_composicion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (composicion1 [(n,n+1) | n <- [1..2000]] [(n,n+1) | n <- [1..2000]])
--    1999
--    (1.54 secs, 770,410,352 bytes)
--    λ> length (composicion2 [(n,n+1) | n <- [1..2000]] [(n,n+1) | n <- [1..2000]])
--    1999
--    (1.66 secs, 1,348,948,096 bytes)
