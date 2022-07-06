-- Transitividad_de_una_relacion.hs
-- Transitividad de una relación.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una relación binaria R sobre un conjunto A es transitiva cuando se
-- cumple que siempre que un elemento se relaciona con otro y éste
-- último con un tercero, entonces el primero se relaciona con el
-- tercero. 
-- 
-- Definir la función
--    transitiva :: Ord a => [(a,a)] -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva. 
-- Por ejemplo,
--    transitiva [(1,1),(1,3),(3,1),(3,3),(5,5)]  ==  True
--    transitiva [(1,1),(1,3),(3,1),(5,5)]        ==  False
--    transitiva [(n,n) | n <- [1..10^4]]         ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Transitividad_de_una_relacion where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M (Map, assocs, empty, insertWith, lookup, map)
import Test.QuickCheck (maxSize, quickCheckWith, stdArgs)

-- 1ª solución
-- ===========

transitiva1 :: Ord a => [(a,a)] -> Bool
transitiva1 r = 
  and [(x,y) `elem` r | (x,u) <- r, (v,y) <- r, u == v]

-- 2ª solución
-- ===========

transitiva2 :: Ord a => [(a,a)] -> Bool
transitiva2 r = 
  all (`elem` r) [(x,y) | (x,u) <- r, (v,y) <- r, u == v]

-- 3ª solución
-- ===========

transitiva3 :: Ord a => [(a,a)] -> Bool
transitiva3 r =
  subconjunto (composicion r r) r

-- (subconjunto xs ys) se verifica si xs es un subconjunto de xs. Por
-- ejemplo, 
--    subconjunto [1,3] [3,1,5]  ==  True
--    subconjunto [3,1,5] [1,3]  ==  False
subconjunto :: Ord a => [a] -> [a] -> Bool
subconjunto xs ys =
  all (`elem`ys) xs

-- (composicion r s) es la composición de las relaciones binarias r y
-- s. Por ejemplo, 
--    λ> composicion [(1,2)] [(2,3),(2,4)]
--    [(1,3),(1,4)]
--    λ> composicion [(1,2),(5,2)] [(2,3),(2,4)]
--    [(1,3),(1,4),(5,3),(5,4)]
--    λ> composicion [(1,2),(1,4),(1,5)] [(2,3),(4,3)]
--    [(1,3)]
composicion :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion r s =
  nub [(x,y) | (x,u) <- r, (v,y) <- s, u == v] 

-- 3ª solución
-- ===========

transitiva4 :: Ord a => [(a,a)] -> Bool
transitiva4 r =
  subconjunto (composicion4 r r) r

composicion4 :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)]
composicion4 r s =
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

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_transitiva :: [(Int,Int)] -> Bool
prop_transitiva r =
  all (== transitiva1 r)
      [transitiva2 r,
       transitiva3 r,
       transitiva4 r] 

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_transitiva
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> transitiva1 [(n,n) | n <- [1..5*10^3]]
--    True
--    (4.27 secs, 1,403,139,032 bytes)
--    λ> transitiva2 [(n,n) | n <- [1..5*10^3]]
--    True
--    (4.24 secs, 1,402,739,056 bytes)
--    λ> transitiva3 [(n,n) | n <- [1..5*10^3]]
--    True
--    (4.41 secs, 1,403,219,880 bytes)
--    λ> transitiva4 [(n,n) | n <- [1..5*10^3]]
--    True
--    (0.46 secs, 16,206,624 bytes)
