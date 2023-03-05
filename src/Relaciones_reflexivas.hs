-- Relaciones_reflexivas.hs
-- Relaciones reflexivas
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una relación binaria R sobre un conjunto A se puede representar
-- mediante un par (xs,ps) donde xs es la lista de los elementos de A
-- (el universo de R) y ps es la lista de pares de R (el grafo de R).
--
-- El tipo de las relaciones binarias sobre a es
--    type Rel a = ([a],[(a,a)])
--
-- Definir la función
--    reflexiva :: Eq a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relación r es reflexiva. Por
-- ejemplo,
--    reflexiva ([1,3],[(1,1),(1,3),(3,3)])    ==  True
--    reflexiva ([1,2,3],[(1,1),(1,3),(3,3)])  ==  False
-- ---------------------------------------------------------------------

module Relaciones_reflexivas where

import Data.List (nub)
import Test.QuickCheck

type Rel a = ([a],[(a,a)])

-- 1ª solución
-- ===========

reflexiva :: Eq a => Rel a -> Bool
reflexiva ([], _)   = True
reflexiva (x:xs, ps) = (x, x) `elem` ps && reflexiva(xs, ps)

-- 2ª solución
-- ===========

reflexiva2 :: Eq a => Rel a -> Bool
reflexiva2 (us,ps) = and [(x,x) `elem` ps | x <- us]

-- 3ª solución
-- ===========

reflexiva3 :: Eq a => Rel a -> Bool
reflexiva3 (us,ps) = all (`elem` ps) [(x,x) | x <- us]

-- 4ª solución
-- ===========

reflexiva4 :: Eq a => Rel a -> Bool
reflexiva4 (us,ps) = all (\x -> (x,x) `elem` ps) us

-- Comprobación de equivalencia
-- ============================

newtype Relacion a = R (Rel a)
  deriving Show

-- Generador de relaciones binarias. Por ejemplo,
--    λ> sample (relacionArbitraria :: Gen (Relacion Int))
--    R ([],[])
--    R ([2,-2],[(2,-2)])
--    R ([-2],[(-2,-2)])
--    R ([7,0,2,6],[(7,0),(7,2),(0,7),(0,0),(2,7),(2,2)])
--    R ([-8,0],[(0,-8)])
--    R ([8,-11,5,-1],[(8,-11),(8,5),(8,-1),(5,-1),(-1,5),(-1,-1)])
--    R ([-2,0],[(-2,-2),(-2,0),(0,-2)])
--    R ([0],[(0,0)])
--    R ([-2,-4,0],[(-2,-2),(-2,-4),(-2,0),(-4,-2),(-4,0)])
--    R ([-3,2,-7,5,8],[(-3,-7),(2,-3),(2,2),(2,-7),(2,5),(-7,-3),(-7,-7),(5,2),(5,5),(5,8),(8,-3),(8,2)])
relacionArbitraria :: (Arbitrary a, Eq a) => Gen (Relacion a)
relacionArbitraria = do
  us1 <- arbitrary
  let us = nub us1
  ps <- sublistOf [(x,y) | x <- us, y <- us]
  return (R (us, ps))

-- Relaciones es una subclase de Arbitrary.
instance (Arbitrary a, Eq a) => Arbitrary (Relacion a) where
  arbitrary = relacionArbitraria

-- La propiedad es
prop_reflexiva :: Relacion Int -> Bool
prop_reflexiva (R r) =
  all (== reflexiva r)
      [reflexiva2 r,
       reflexiva3 r,
       reflexiva4 r]

-- La comprobación es
--    λ> quickCheck prop_reflexiva
--    +++ OK, passed 100 tests.
