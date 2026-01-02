-- Ordenacion_segun.hs
-- Ordenación según una función.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-Enero-2015 (actualizado 2-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ordenaSegun :: Ord b => (a -> b) -> [a] -> [a]
-- tal que (ordenaSegun f xs) es la lista obtenida ordenando los
-- elementos de xs según sus valores mediante la función f. Por ejemplo,
--    ordenaSegun abs [-3,2,5,-2]                           ==  [2,-2,-3,5]
--    ordenaSegun abs [-3,-2,5,2]                           ==  [-2,2,-3,5]
--    ordenaSegun length ["pq","x","mn"]                    ==  ["x","pq","mn"]
--    [g 0 | g <- ordenaSegun (\f -> f 4) [(+5),(+2),(+3)]] == [2,3,5]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Ordenacion_segun where

import Data.List (sort, sortBy, sortOn)
import Data.Ord (comparing)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

ordenaSegun1 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun1 _ []  = []
ordenaSegun1 f (x:xs) = insertaSegun f x (ordenaSegun1 f xs)

insertaSegun :: Ord b => (a -> b) -> a -> [a] -> [a]
insertaSegun _ x [] = [x]
insertaSegun f x (y:ys) | f x <= f y = x : y : ys
                        | otherwise  = y : insertaSegun f x ys

-- 2ª solución
-- ===========

ordenaSegun2 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun2 _ [] = []
ordenaSegun2 f (x:xs) = ordenaSegun2 f menores ++ [x] ++ ordenaSegun2 f mayores
  where
    menores = [y | y <- xs, f y < f x]
    mayores = [y | y <- xs, f y >= f x]

-- 3ª solución
-- ===========

ordenaSegun3 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun3 = sortBy . comparing

-- 4ª solución
-- ===========

ordenaSegun4 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun4 f xs = [xs!!i | (_,i) <- sort (zip [f x | x <- xs] [0..])]

-- 5ª solución
-- ===========

ordenaSegun5 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun5 f xs = map ((xs!!) . snd) (sort (zip (map f xs) [0..]))

-- 6ª solución
-- ===========

ordenaSegun6 :: Ord b => (a -> b) -> [a] -> [a]
ordenaSegun6 = sortOn

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Int) -> [Int] -> [Int]) -> Spec
specG ordenaSegun = do
  it "e1" $
    ordenaSegun abs [-3,2,5,-2] `shouldBe` [2,-2,-3,5]
  it "e1" $
    ordenaSegun abs [-3,-2,5,2] `shouldBe` [-2,2,-3,5]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenaSegun1
  describe "def. 2" $ specG ordenaSegun2
  describe "def. 3" $ specG ordenaSegun3
  describe "def. 4" $ specG ordenaSegun4
  describe "def. 5" $ specG ordenaSegun5
  describe "def. 6" $ specG ordenaSegun6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: (Int -> Int) -> [Int] -> Bool
prop_equivalencia f xs =
  all (== ordenaSegun1 f xs)
      [ordenaSegun2 f xs,
       ordenaSegun4 f xs,
       ordenaSegun5 f xs,
       ordenaSegun6 f xs]

-- La comprobación es
--    λ> quickCheck' prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (ordenaSegun1 (\x -> sum [1..x]) [400,399..1])
--    400
--    (1.17 secs, 2,860,135,896 bytes)
--    λ> length (ordenaSegun2 (\x -> sum [1..x]) [400,399..1])
--    400
--    (2.08 secs, 5,708,849,176 bytes)
--    λ> length (ordenaSegun3 (\x -> sum [1..x]) [400,399..1])
--    400
--    (0.02 secs, 7,902,288 bytes)
--    λ> length (ordenaSegun4 (\x -> sum [1..x]) [400,399..1])
--    400
--    (0.03 secs, 7,873,376 bytes)
--    λ> length (ordenaSegun5 (\x -> sum [1..x]) [400,399..1])
--    400
--    (0.03 secs, 14,834,200 bytes)
--    λ> length (ordenaSegun6 (\x -> sum [1..x]) [400,399..1])
--    400
--    (0.02 secs, 7,840,776 bytes)
--
--    λ> length (ordenaSegun3 (\x -> sum [1..x]) [14000,13999..1])
--    14000
--    (2.48 secs, 8,633,839,808 bytes)
--    λ> length (ordenaSegun4 (\x -> sum [1..x]) [14000,13999..1])
--    14000
--    (2.45 secs, 8,632,831,696 bytes)
--    λ> length (ordenaSegun5 (\x -> sum [1..x]) [14000,13999..1])
--    14000
--    (4.80 secs, 17,254,086,920 bytes)
--    λ> length (ordenaSegun6 (\x -> sum [1..x]) [14000,13999..1])
--    14000
--    (2.51 secs, 8,631,711,096 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_ordenaSegun :: [Int] -> Bool
prop_ordenaSegun xs =
  ordenaSegun1 id xs == sort xs

-- La comprobación es
--    λ> quickCheck prop_ordenaSegun
--    +++ OK, passed 100 tests.
