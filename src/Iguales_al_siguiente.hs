-- Iguales_al_siguiente.hs
-- Iguales al siguiente.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    igualesAlSiguiente :: Eq a => [a] -> [a]
-- tal que (igualesAlSiguiente xs) es la lista de los elementos de xs
-- que son iguales a su siguiente. Por ejemplo,
--    igualesAlSiguiente [1,2,2,2,3,3,4]  ==  [2,2,3]
--    igualesAlSiguiente [1..10]          ==  []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Iguales_al_siguiente where

import Data.List (group)
import Test.QuickCheck
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

igualesAlSiguiente1 :: Eq a => [a] -> [a]
igualesAlSiguiente1 xs =
  [x | (x, y) <- consecutivos1 xs, x == y]

-- (consecutivos1 xs) es la lista de pares de elementos consecutivos en
-- xs. Por ejemplo,
--    consecutivos1 [3,5,2,7]  ==  [(3,5),(5,2),(2,7)]
consecutivos1 :: [a] -> [(a, a)]
consecutivos1 xs = zip xs (tail xs)

-- 2ª solución
-- ===========

igualesAlSiguiente2 :: Eq a => [a] -> [a]
igualesAlSiguiente2 xs =
  [x | (x,y) <- consecutivos2 xs, x == y]

-- (consecutivos2 xs) es la lista de pares de elementos consecutivos en
-- xs. Por ejemplo,
--    consecutivos2 [3,5,2,7]  ==  [(3,5),(5,2),(2,7)]
consecutivos2 :: [a] -> [(a, a)]
consecutivos2 (x:y:zs) = (x,y) : consecutivos2 (y:zs)
consecutivos2 _        = []

-- 3ª solución
-- ===========

igualesAlSiguiente3 :: Eq a => [a] -> [a]
igualesAlSiguiente3 (x:y:zs) | x == y    = x : igualesAlSiguiente3 (y:zs)
                             | otherwise = igualesAlSiguiente3 (y:zs)
igualesAlSiguiente3 _                    = []

-- 4ª solución
-- ===========

igualesAlSiguiente4 :: Eq a => [a] -> [a]
igualesAlSiguiente4 xs = concat [ys | (_:ys) <- group xs]

-- 5ª solución
-- ===========

igualesAlSiguiente5 :: Eq a => [a] -> [a]
igualesAlSiguiente5 xs = concat (map tail (group xs))

-- 6ª solución
-- ===========

igualesAlSiguiente6 :: Eq a => [a] -> [a]
igualesAlSiguiente6 xs = concatMap tail (group xs)

-- 7ª solución
-- ===========

igualesAlSiguiente7 :: Eq a => [a] -> [a]
igualesAlSiguiente7 = concatMap tail . group

-- 8ª solución
-- ===========

igualesAlSiguiente8 :: Eq a => [a] -> [a]
igualesAlSiguiente8 xs = tail =<< group xs

-- 9ª solución
-- ===========

igualesAlSiguiente9 :: Eq a => [a] -> [a]
igualesAlSiguiente9 = (tail =<<) . group

-- 10ª solución
-- ===========

igualesAlSiguiente10 :: Eq a => [a] -> [a]
igualesAlSiguiente10 xs = aux xs (tail xs)
  where aux (u:us) (v:vs) | u == v    = u : aux us vs
                          | otherwise = aux us vs
        aux _ _ = []

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG igualesAlSiguiente = do
  it "e1" $
    igualesAlSiguiente [1,2,2,2,3,3,4] `shouldBe` [2,2,3]
  it "e2" $
    igualesAlSiguiente [1..10] `shouldBe` []


spec :: Spec
spec = do
  describe "def. 1" $ specG igualesAlSiguiente1
  describe "def. 2" $ specG igualesAlSiguiente2
  describe "def. 3" $ specG igualesAlSiguiente3
  describe "def. 4" $ specG igualesAlSiguiente4
  describe "def. 5" $ specG igualesAlSiguiente5
  describe "def. 6" $ specG igualesAlSiguiente6
  describe "def. 7" $ specG igualesAlSiguiente7
  describe "def. 8" $ specG igualesAlSiguiente8
  describe "def. 9" $ specG igualesAlSiguiente9
  describe "def. 10" $ specG igualesAlSiguiente10

-- La verificación es
--    λ> verifica
--    20 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_igualesAlSiguiente :: [Int] -> Bool
prop_igualesAlSiguiente xs =
  all (== igualesAlSiguiente1 xs)
      [igualesAlSiguiente2 xs,
       igualesAlSiguiente3 xs,
       igualesAlSiguiente4 xs,
       igualesAlSiguiente5 xs,
       igualesAlSiguiente6 xs,
       igualesAlSiguiente7 xs,
       igualesAlSiguiente8 xs,
       igualesAlSiguiente9 xs,
       igualesAlSiguiente10 xs]

-- La comprobación es
--    λ> quickCheck prop_igualesAlSiguiente
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    > ej = concatMap show [1..10^6]
--    (0.01 secs, 446,752 bytes)
--    λ> length ej
--    5888896
--    (0.16 secs, 669,787,856 bytes)
--    λ> length (show (igualesAlSiguiente1 ej))
--    588895
--    (1.30 secs, 876,867,992 bytes)
--    λ> length (show (igualesAlSiguiente2 ej))
--    588895
--    (1.91 secs, 1,724,868,880 bytes)
--    λ> length (show (igualesAlSiguiente3 ej))
--    588895
--    (1.34 secs, 1,168,957,152 bytes)
--    λ> length (show (igualesAlSiguiente4 ej))
--    588895
--    (1.22 secs, 1,922,735,352 bytes)
--    λ> length (show (igualesAlSiguiente5 ej))
--    588895
--    (0.45 secs, 2,007,535,504 bytes)
--    λ> length (show (igualesAlSiguiente6 ej))
--    588895
--    (0.43 secs, 1,541,135,200 bytes)
--    λ> length (show (igualesAlSiguiente7 ej))
--    588895
--    (0.39 secs, 1,541,135,384 bytes)
--    λ> length (show (igualesAlSiguiente8 ej))
--    588895
--    (0.39 secs, 1,541,135,160 bytes)
--    λ> length (show (igualesAlSiguiente9 ej))
--    588895
--    (0.37 secs, 1,541,135,416 bytes)
--    λ> length (show (igualesAlSiguiente10 ej))
--    588895
--    (1.16 secs, 744,956,960 bytes)
