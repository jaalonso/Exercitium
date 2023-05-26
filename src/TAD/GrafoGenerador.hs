-- GrafoGenerador.hs
-- eneradores de grafos arbitrarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 03-junio-2023
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TAD.GrafoGenerador where

import TAD.Grafo (Grafo, Orientacion (D, ND), creaGrafo)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, vectorOf)

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    λ> generaGND 3 [4,2,5]
--    G ND ([1,2,3],[((1,2),4),((1,3),2),((2,3),5)])
--    λ> generaGND 3 [4,-2,5]
--    G ND ([1,2,3],[((1,2),4),((2,3),5)])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
  where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
        l2 = zip l1 ps
        l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    λ> generaGD 3 [4,2,5]
--    G D ([1,2,3],[((1,1),4),((1,2),2),((1,3),5)])
--    λ> generaGD 3 [4,2,5,3,7,9,8,6]
--    G D ([1,2,3],[((1,1),4),((1,2),2),((1,3),5),
--                  ((2,1),3),((2,2),7),((2,3),9),
--                  ((3,1),8),((3,2),6)])
generaGD :: Int -> [Int] -> Grafo Int Int
generaGD n ps = creaGrafo D (1,n) l3
  where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
        l2 = zip l1 ps
        l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    λ> sample genGD
--    G D ([1],[])
--    G D ([1,2],[((1,1),5),((2,1),4)])
--    G D ([1,2],[((1,1),3),((1,2),3)])
--    G D ([1,2,3,4,5,6],[])
--    G D ([1,2],[((2,2),16)])
--    ...
genGD :: Gen (Grafo Int Int)
genGD = do
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    λ> sample genGND
--    G ND ([1,2,3,4,5,6,7,8],[])
--    G ND ([1],[])
--    G ND ([1,2,3,4,5],[((1,2),2),((2,3),5),((3,4),5),((3,5),5)])
--    G ND ([1,2,3,4,5],[((1,2),6),((1,3),5),((1,5),1),((3,5),9),((4,5),6)])
--    G ND ([1,2,3,4],[((1,2),5),((3,4),2)])
--    G ND ([1,2,3],[])
--    G ND ([1,2,3,4],[((1,2),5),((1,4),14),((2,4),10)])
--    G ND ([1,2,3,4,5],[((1,5),8),((4,5),5)])
--    G ND ([1,2,3,4],[((1,2),1),((1,4),4),((2,3),4),((3,4),5)])
--    G ND ([1,2,3],[((1,2),8),((1,3),8),((2,3),3)])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    λ> sample genG
--    G ND ([1,2,3,4,5,6],[])
--    G D ([1],[((1,1),2)])
--    G D ([1,2],[((1,1),9)])
--    ...
genG :: Gen (Grafo Int Int)
genG = do
  d <- choose (True,False)
  n <- choose (1,10)
  xs <- vectorOf (n*n) arbitrary
  if d then return (generaGD n xs)
       else return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente.
instance Arbitrary (Grafo Int Int) where
  arbitrary = genG
