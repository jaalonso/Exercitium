-- Mezcla_de_infinitas_listas_infinitas.hs
-- Mezcla de infinitas listas infinitas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-Enero-2015 (actualizado 1-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mezclaTodas :: Ord a => [[a]] -> [a]
-- tal que (mezclaTodas xss) es la mezcla ordenada de xss, donde tanto
-- xss como sus elementos son listas infinitas ordenadas. Por ejemplo,
--    λ> take 10 (mezclaTodas [[n,2*n..] | n <- [2..]])
--    [2,3,4,5,6,7,8,9,10,11]
--    λ> take 10 (mezclaTodas [[n,2*n..] | n <- [2,9..]])
--    [2,4,6,8,9,10,12,14,16,18]
--    λ> mezclaTodas [[n,2*n..] | n <- [2..]] !! 50000
--    50002
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mezcla_de_infinitas_listas_infinitas where

import Data.List (insert, nub)
import Data.List.Ordered (unionAll)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mezclaTodas1 :: Ord a => [[a]] -> [a]
mezclaTodas1 ((x:xs):xss) = x : mezcla xs (mezclaTodas1 xss)

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys)
  | x < y      = x : mezcla xs (y:ys)
  | x == y     = x : mezcla xs ys
  | otherwise  = y : mezcla (x:xs) ys

-- 2ª solución
-- ===========

mezclaTodas2 :: Ord a => [[a]] -> [a]
mezclaTodas2 = foldr1 xmezcla
  where xmezcla (x:xs) ys = x : mezcla2 xs ys

mezcla2 :: Ord a => [a] -> [a] -> [a]
mezcla2 (x:xs) (y:ys) =
  case compare x y of
    LT -> x : mezcla2 xs (y:ys)
    EQ -> x : mezcla2 xs ys
    GT -> y : mezcla2 (x:xs) ys

-- 3ª solución
-- ===========

mezclaTodas3 :: Ord a => [[a]] -> [a]
mezclaTodas3 ((x:xs):rs) =
  nub (x : mezclaTodas3 (insert xs rs))

-- 4ª solución
-- ===========

mezclaTodas4 :: Ord a => [[a]] -> [a]
mezclaTodas4 ((x:xs):xss) = x : mezcla2 xs (mezclaTodas xss)
  where
    mezclaTodas ((y:ys):yss) = y : mezcla2 ys (mezclaTodas (mezclaPares yss))
    mezclaPares (l1:l2:ls) = mezcla2 l1 l2 : mezclaPares ls
    mezclaPares ls         = ls

-- 5ª solución
-- ===========

mezclaTodas5 :: Ord a => [[a]] -> [a]
mezclaTodas5 = unionAll

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [Int]) -> Spec
specG mezclaTodas = do
  it "e1" $
    take 10 (mezclaTodas [[n,2*n..] | n <- [2..]])
    `shouldBe` [2,3,4,5,6,7,8,9,10,11]
  it "e2" $
    take 10 (mezclaTodas [[n,2*n..] | n <- [2,9..]])
    `shouldBe` [2,4,6,8,9,10,12,14,16,18]

spec :: Spec
spec = do
  describe "def. 1" $ specG mezclaTodas1
  describe "def. 2" $ specG mezclaTodas2
  describe "def. 3" $ specG mezclaTodas3
  describe "def. 4" $ specG mezclaTodas4
  describe "def. 5" $ specG mezclaTodas5

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Genera una lista infinita ordenada de listas infinitas
-- ordenadas. Por ejemplo,
--    λ> take 5 . map (take 3) <$> generate genListas
--    [[11,13,15],[12,15,18],[15,18,21],[18,21,24],[19,20,21]]
genListas :: Gen [[Int]]
genListas = do
  as <- infiniteList :: Gen [Int]
  ds <- infiniteList :: Gen [Int]
  let
    incs = [ (abs n `mod` 5) + 1 | n <- as ]
    cabezas = scanl1 (+) incs
    pasos = [ (abs n `mod` 10) + 1 | n <- ds ]
    xss = zipWith (\c p -> [c, c+p ..]) cabezas pasos
  return xss

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia = forAll genListas $ \xss ->
  all (== take 100 (mezclaTodas1 xss))
      [take 100 (mezclaTodas2 xss),
       take 100 (mezclaTodas4 xss),
       take 100 (mezclaTodas5 xss)]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mezclaTodas1 [[n,2*n..] | n <- [2..]] !! 600
--    602
--    (0.14 secs, 86,198,648 bytes)
--    λ> mezclaTodas2 [[n,2*n..] | n <- [2..]] !! 600
--    602
--    (0.09 secs, 79,821,032 bytes)
--    λ> mezclaTodas3 [[n,2*n..] | n <- [2..]] !! 600
--    602
--    (2.04 secs, 170,432,048 bytes)
--    λ> mezclaTodas4 [[n,2*n..] | n <- [2..]] !! 600
--    602
--    (0.04 secs, 9,359,616 bytes)
--    λ> mezclaTodas5 [[n,2*n..] | n <- [2..]] !! 600
--    602
--    (0.01 secs, 3,297,384 bytes)
--
--    λ> mezclaTodas1 [[n,2*n..] | n <- [2..]] !! 3000
--    3002
--    (2.45 secs, 2,320,688,264 bytes)
--    λ> mezclaTodas2 [[n,2*n..] | n <- [2..]] !! 3000
--    3002
--    (1.65 secs, 2,172,612,408 bytes)
--    λ> mezclaTodas4 [[n,2*n..] | n <- [2..]] !! 3000
--    3002
--    (0.10 secs, 61,177,480 bytes)
--    λ> mezclaTodas5 [[n,2*n..] | n <- [2..]] !! 3000
--    3002
--    (0.02 secs, 16,561,096 bytes)
--
--    λ> mezclaTodas4 [[n,2*n..] | n <- [2..]] !! 50000
--    50002
--    (2.10 secs, 1,706,343,488 bytes)
--    λ> mezclaTodas5 [[n,2*n..] | n <- [2..]] !! 50000
--    50002
--    (0.50 secs, 394,000,128 bytes)
