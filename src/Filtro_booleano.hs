-- Filtro_booleano.hs
-- Filtro booleano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-Junio-2014 (actualizado 1-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    filtroBooleano :: [Bool] -> [a] -> [Maybe a]
-- tal que (filtroBooleano xs ys) es la lista de los elementos de ys
-- tales que el elemento de xs en la misma posición es verdadero. Por
-- ejemplo,
--    λ> filtroBooleano [True,False,True] "Sevilla"
--    [Just 'S',Nothing,Just 'v']
--    λ> filtroBooleano (repeat True) "abc"
--    [Just 'a',Just 'b',Just 'c']
--    λ> take 3 (filtroBooleano (repeat True) [1..])
--    [Just 1,Just 2,Just 3]
--    λ> take 3 (filtroBooleano (repeat False) [1..])
--    [Nothing,Nothing,Nothing]
--    λ> take 3 (filtroBooleano (cycle [True,False]) [1..])
--    [Just 1,Nothing,Just 3]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Filtro_booleano where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

filtroBooleano1 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano1 xs ys = [f x y | (x,y) <- zip xs ys]
    where f True y = Just y
          f _    _ = Nothing

-- 2ª solución
-- ===========

filtroBooleano2 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano2 = zipWith f
    where f True y = Just y
          f _    _ = Nothing

-- 3ª solución
-- ===========

filtroBooleano3 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano3 = zipWith (\x y -> if x then Just y else Nothing)

-- 4ª solución
-- ===========

filtroBooleano4 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano4 (x:xs) (y:ys) | x         = Just y : filtroBooleano4 xs ys
                              | otherwise = Nothing : filtroBooleano4 xs ys
filtroBooleano4 _ _                       = []

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Bool] -> [Int] -> [Maybe Int]) -> Spec
specG filtroBooleano = do
  it "e1" $
    take 3 (filtroBooleano (repeat True) [1..])
    `shouldBe` [Just 1,Just 2,Just 3]
  it "e2" $
    take 3 (filtroBooleano (repeat False) [1..])
    `shouldBe` [Nothing,Nothing,Nothing]
  it "e3" $
    take 3 (filtroBooleano (cycle [True,False]) [1..])
    `shouldBe` [Just 1,Nothing,Just 3]

spec :: Spec
spec = do
  describe "def. 1" $ specG filtroBooleano1
  describe "def. 2" $ specG filtroBooleano2
  describe "def. 3" $ specG filtroBooleano3
  describe "def. 4" $ specG filtroBooleano4

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_filtroBooleano :: [Bool] -> [Int] -> Bool
prop_filtroBooleano xs ys =
  all (== filtroBooleano1 xs ys)
      [filtroBooleano2 xs ys,
       filtroBooleano3 xs ys,
       filtroBooleano4 xs ys]

-- La verificación es
--    λ> quickCheck prop_filtroBooleano
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (take 10000000 (filtroBooleano1 (cycle [True,False]) [1..]))
--    10000000
--    (2.15 secs, 3,360,601,632 bytes)
--    λ> length (take 10000000 (filtroBooleano2 (cycle [True,False]) [1..]))
--    10000000
--    (0.38 secs, 2,320,601,496 bytes)
--    λ> length (take 10000000 (filtroBooleano3 (cycle [True,False]) [1..]))
--    10000000
--    (0.38 secs, 2,320,601,520 bytes)
--    λ> length (take 10000000 (filtroBooleano4 (cycle [True,False]) [1..]))
--    10000000
--    (3.20 secs, 2,800,602,664 bytes)
