-- Clausura_transitiva_de_una_relacion_binaria.hs
-- Clausura transitiva de una relación binaria.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La clausura transitiva de una relación binaria R es la menor
-- relación transitiva que contiene a R. Se puede calcular usando la
-- composición de relaciones. Veamos un ejemplo, en el que (R ∘ S)
-- representa la composición de R y S: sea 
--    R = [(1,2),(2,5),(5,6)]
-- la relación R no es transitiva ya que (1,2) y (1,5) pertenecen a R
-- pero (1,5) no pertenece; sea
--    R1 = R ∪ (R ∘ R)
--       = [(1,2),(2,5),(5,6),(1,5),(2,6)]
-- la relación R1 tampoco es transitiva ya que (1,2) y (2,6) pertenecen a R
-- pero (1,6) no pertenece; sea
--    R2 = R1 ∪ (R1 ∘ R1)
--       = [(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)]
-- La relación R2 es transitiva y contiene a R. Además, R2 es la
-- clausura transitiva de R.
--
-- Definir la función
--    clausuraTransitiva :: Ord a => [(a,a)] -> [(a,a)]   
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relación transitiva que contiene a r. Por ejemplo,
--    λ> clausuraTransitiva [(1,2),(2,5),(5,6)]
--    [(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)]
--    λ> clausuraTransitiva [(1,2),(2,5),(5,6),(6,3)]
--    [(1,2),(2,5),(5,6),(6,3),(1,5),(2,6),(5,3),(1,6),(2,3),(1,3)]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Clausura_transitiva_de_una_relacion_binaria where

import Data.List (union, nub, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M (Map, assocs, empty, insertWith, lookup, map)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

clausuraTransitiva1 :: Ord a => [(a,a)] -> [(a,a)]  
clausuraTransitiva1 r
  | transitiva r = r
  | otherwise    = clausuraTransitiva1 r1
  where r1 = r `union` composicion r r

-- (transitiva r) se verifica si la relación r es transitiva. Por
-- ejemplo, 
--    transitiva [(1,1),(1,3),(3,1),(3,3),(5,5)]  ==  True
--    transitiva [(1,1),(1,3),(3,1),(5,5)]        ==  False
transitiva :: Ord a => [(a,a)] -> Bool
transitiva r = subconjunto (composicion r r) r

-- (composicion r s) es la composición de las relaciones binarias r y
-- s. Por ejemplo, 
--    λ> composicion [(1,2)] [(2,3),(2,4)]
--    [(1,3),(1,4)]
--    λ> composicion [(1,2),(5,2)] [(2,3),(2,4)]
--    [(1,3),(1,4),(5,3),(5,4)]
--    λ> composicion [(1,2),(1,4),(1,5)] [(2,3),(4,3)]
--    [(1,3)]
composicion :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion r s = nub [(x,y) | (x,u) <- r, (v,y) <- s, u == v] 

-- (subconjunto xs ys) se verifica si xs es un subconjunto de xs. Por
-- ejemplo, 
--    subconjunto [1,3] [3,1,5]  ==  True
--    subconjunto [3,1,5] [1,3]  ==  False
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto xs ys = all (`elem` ys) xs

-- 2ª solución
-- =============

clausuraTransitiva2 :: Ord a => [(a,a)] -> [(a,a)]  
clausuraTransitiva2 r
  | r1 == r   = r
  | otherwise = clausuraTransitiva2 r1
  where r1 = r `union` composicion r r

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_clausuraTransitiva :: [(Int,Int)] -> Bool
prop_clausuraTransitiva r =
  all (== sort (clausuraTransitiva1 r))
      [sort (clausuraTransitiva2 r),
       sort (clausuraTransitiva3 r)]
  
-- La comprobación es
--    λ> quickCheck prop_clausuraTransitiva
--    +++ OK, passed 100 tests.

-- 3ª solución
-- ===========

clausuraTransitiva3 :: Ord a => [(a,a)] -> [(a,a)]  
clausuraTransitiva3 r
  | transitiva3 r = r
  | otherwise     = clausuraTransitiva3 r1
  where r1 = r `union` composicion3 r r

transitiva3 :: Ord a => [(a,a)] -> Bool
transitiva3 r = subconjunto (composicion3 r r) r

composicion3 :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion3 r s =
  relAlista (composicionRel (listaArel r) (listaArel s))

-- Una relación se puede representar por un diccionario donde las claves
-- son los elementos y los valores son las listas de los elementos con
-- los que se relaciona.
type Rel a = M.Map a [a]

-- (listaArel xys) es la relación correspondiente a la lista de pares
-- xys. Por ejemplo.
--    λ> listaArel [(1,1),(1,2),(1,3),(1,6),(2,2),(2,6),(3,3),(3,6),(6,6)]
--    fromList [(1,[1,2,3,6]),(2,[2,6]),(3,[3,6]),(6,[6])]
listaArel :: Ord a => [(a,a)] -> Rel a
listaArel []          = M.empty
listaArel ((x,y):xys) = M.insertWith (++) x [y] (listaArel xys)

-- (composicionRel r s) es la composición de las relaciones r y s. Por
-- ejemplo,
--    λ> r = listaArel [(1,2),(5,2)]
--    λ> s = listaArel [(2,3),(2,4)]
--    λ> composicionRel r s
--    fromList [(1,[3,4]),(5,[3,4])]
composicionRel :: Ord a => Rel a -> Rel a -> Rel a
composicionRel r s =
  M.map f r 
  where f xs = concat (mapMaybe (`M.lookup` s) xs)

-- (relAlista r) es la lista de pares correspondientes a la relación
-- r. Por ejemplo,
--    λ> relAlista (M.fromList [(1,[3,4]),(5,[3,4])])
--    [(1,3),(1,4),(5,3),(5,4)]
relAlista :: Ord a => Rel a -> [(a,a)]
relAlista r =
  nub [(x,y) | (x,ys) <- M.assocs r, y <- ys] 

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (clausuraTransitiva1 [(n,n+1) | n <- [1..60]])
--    1830
--    (2.15 secs, 453,533,992 bytes)
--    λ> length (clausuraTransitiva2 [(n,n+1) | n <- [1..60]])
--    1830
--    (2.23 secs, 558,571,904 bytes)
--    λ> length (clausuraTransitiva3 [(n,n+1) | n <- [1..60]])
--    1830
--    (0.25 secs, 207,168,552 bytes)
