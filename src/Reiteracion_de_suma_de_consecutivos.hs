-- Reiteracion_de_suma_de_consecutivos.hs
-- Reiteración de suma de consecutivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-marzo-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La reiteración de la suma de los elementos consecutivos de la lista
-- [1,5,3] es 14 como se explica en el siguiente diagrama
--    1 + 5 = 6
--              \
--               ==> 14
--              /
--    5 + 3 = 8
-- y la de la lista [1,5,3,4] es 29 como se explica en el siguiente
-- diagrama
--    1 + 5 = 6
--              \
--               ==> 14
--              /       \
--    5 + 3 = 8          ==> 29
--              \       /
--               ==> 15
--              /
--    3 + 4 = 7
--
-- Definir la función
--    sumaReiterada :: Num a => [a] -> a
-- tal que (sumaReiterada xs) es la suma reiterada de los elementos
-- consecutivos de la lista no vacía xs. Por ejemplo,
--    sumaReiterada [1,5,3]    ==  14
--    sumaReiterada [1,5,3,4]  ==  29
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Reiteracion_de_suma_de_consecutivos where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaReiterada1 :: Num a => [a] -> a
sumaReiterada1 [x] = x
sumaReiterada1 xs  = sumaReiterada1 [x+y | (x,y) <- consecutivos xs]

-- (consecutivos xs) es la lista de pares de elementos consecutivos de
-- xs. Por ejemplo,
--    consecutivos [1,5,3,4]  ==  [(1,5),(5,3),(3,4)]
consecutivos :: [a] -> [(a,a)]
consecutivos xs = zip xs (tail xs)

-- 2ª solución
-- ===========

sumaReiterada2 :: Num a => [a] -> a
sumaReiterada2 [x] = x
sumaReiterada2 xs  = sumaReiterada2 (sumaConsecutivos xs)

-- (sumaConsecutivos xs) es la suma de los de pares de elementos
-- consecutivos de xs. Por ejemplo,
--    sumaConsecutivos [1,5,3,4]   ==  [6,8,7]
sumaConsecutivos :: Num a => [a] -> [a]
sumaConsecutivos xs = zipWith (+) xs (tail xs)

-- 3ª solución
-- ===========

sumaReiterada3 :: Num a => [a] -> a
sumaReiterada3 [x] = x
sumaReiterada3 xs  = sumaReiterada3 (zipWith (+) xs (tail xs))

-- 4ª solución
-- ===========

sumaReiterada4 :: Num a => [a] -> a
sumaReiterada4 [x]    = x
sumaReiterada4 (x:xs) = sumaReiterada4 (zipWith (+) (x:xs) xs)

-- 5ª solución
-- ===========

sumaReiterada5 :: Num a => [a] -> a
sumaReiterada5 [x]       = x
sumaReiterada5 xs@(_:ys) = sumaReiterada5 (zipWith (+) xs ys)

-- 6ª solución
-- ===========

sumaReiterada6 :: Num a => [a] -> a
sumaReiterada6 xs =
  head (head (dropWhile noEsUnitaria (iterate sumaConsecutivos xs)))

-- (noEsUnitaria xs) se verifica si la lista xs no tiene sólo un
-- elemento. Por ejemplo,
--    noEsUnitaria []     ==  True
--    noEsUnitaria [7,5]  ==  True
--    noEsUnitaria [7]    ==  False
noEsUnitaria :: [a] -> Bool
noEsUnitaria [_] = False
noEsUnitaria _   = True

-- 7ª solución
-- ===========

sumaReiterada7 :: Num a => [a] -> a
sumaReiterada7 =
  head . head . dropWhile (not . null . tail) . iterate sumaConsecutivos

-- 8ª solución
-- ===========

sumaReiterada8 :: Num a => [a] -> a
sumaReiterada8 =
  head . head . dropWhile (not . null . tail) . iterate (zipWith (+) =<< tail)

-- 9ª solución
-- ===========

sumaReiterada9 :: Num a => [a] -> a
sumaReiterada9 = head . until ((==1) . length) (zipWith (+) <*> tail)

-- 10ª solución
-- ===========

sumaReiterada10 :: Num a => [a] -> a
sumaReiterada10 xs =
  sum (zipWith (*) xs (map fromIntegral (pascal !! (length xs - 1))))

-- pascal es la lista de las filas del triángulo de Pascal. Por ejemplo,
--    λ> take 7 pascal
--    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]
pascal :: [[Integer]]
pascal = [1] : map f pascal
  where f xs = zipWith (+) (0:xs) (xs++[0])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Integer] -> Integer) -> Spec
specG sumaReiterada = do
  it "e1" $
    sumaReiterada [1,5,3] `shouldBe` 14
  it "e2" $
    sumaReiterada [1,5,3,4] `shouldBe` 29

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaReiterada1
  describe "def. 2" $ specG sumaReiterada2
  describe "def. 3" $ specG sumaReiterada3
  describe "def. 4" $ specG sumaReiterada4
  describe "def. 5" $ specG sumaReiterada5
  describe "def. 6" $ specG sumaReiterada6
  describe "def. 7" $ specG sumaReiterada7
  describe "def. 8" $ specG sumaReiterada8
  describe "def. 9" $ specG sumaReiterada9
  describe "def. 10" $ specG sumaReiterada10

-- La verificación es
--    λ> verifica
--
--    20 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_sumaReiterada :: [Integer] -> Property
prop_sumaReiterada xs =
  not (null xs) ==>
  all (== (sumaReiterada1 xs))
      [f xs | f <- [sumaReiterada2,
                    sumaReiterada3,
                    sumaReiterada4,
                    sumaReiterada5,
                    sumaReiterada6,
                    sumaReiterada7,
                    sumaReiterada8,
                    sumaReiterada9,
                    sumaReiterada10 ]]

-- La comprobación es
--    λ> quickCheck prop_sumaReiterada
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (sumaReiterada1 [1..4000]))
--    1208
--    (4.84 secs, 4,444,754,000 bytes)
--    λ> length (show (sumaReiterada2 [1..4000]))
--    1208
--    (3.07 secs, 3,332,858,616 bytes)
--    λ> length (show (sumaReiterada3 [1..4000]))
--    1208
--    (3.04 secs, 3,270,112,112 bytes)
--    λ> length (show (sumaReiterada4 [1..4000]))
--    1208
--    (3.05 secs, 3,332,857,768 bytes)
--    λ> length (show (sumaReiterada5 [1..4000]))
--    1208
--    (3.08 secs, 3,332,570,672 bytes)
--    λ> length (show (sumaReiterada6 [1..4000]))
--    1208
--    (3.03 secs, 3,270,469,704 bytes)
--    λ> length (show (sumaReiterada7 [1..4000]))
--    1208
--    (3.03 secs, 3,270,598,416 bytes)
--    λ> length (show (sumaReiterada8 [1..4000]))
--    1208
--    (3.14 secs, 3,202,183,352 bytes)
--    λ> length (show (sumaReiterada9 [1..4000]))
--    1208
--    (3.71 secs, 2,869,137,232 bytes)
--    λ> length (show (sumaReiterada10 [1..4000]))
--    1208
--    (6.48 secs, 4,621,303,752 bytes)
