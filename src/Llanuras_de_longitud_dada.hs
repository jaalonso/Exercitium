-- Llanuras_de_longitud_dada.hs
-- Llanuras de longitud dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-Diciembre-2014 (actualizado 27-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una llanura de longitud n de una lista xs es una sublista de xs
-- formada por n elementos iguales.
--
-- Definir la función
--    llanuras :: Eq a => Int -> [a] -> [[a]]
-- tal que (llanuras n xs) es la lista de las llanuras de xs que tienen
-- n elementos como mínimo. Por ejemplo,
--    llanuras 3 "aabbbcddddffxffxx"  ==  ["bbb","dddd"]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Llanuras_de_longitud_dada where

import Data.List (group)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

llanuras1 :: Eq a => Int -> [a] -> [[a]]
llanuras1 n xs = [ys | ys <- group xs, length ys >= n]

-- 2ª solución
-- ===========

llanuras2 :: Eq a => Int -> [a] -> [[a]]
llanuras2 n = filter ((>= n) . length) . group

-- 3ª solución
-- ===========

llanuras3 :: Eq a => Int -> [a] -> [[a]]
llanuras3 _ [] = []
llanuras3 n xs@(x:_)
  | length ys >= n = ys : llanuras3 n (dropWhile (x==) xs)
  | otherwise      = llanuras3 n (dropWhile (x==) xs)
  where ys = takeWhile (x==) xs

-- 4ª solución
-- ===========

llanuras4 :: Eq a => Int -> [a] -> [[a]]
llanuras4 _ [] = []
llanuras4 n xs@(x:_)
  | length ys >= n = ys : llanuras4 n zs
  | otherwise      = llanuras4 n zs
  where (ys,zs) = span (x==) xs

-- 5ª solución
-- ===========

llanuras5 :: Eq a => Int -> [a] -> [[a]]
llanuras5 n = aux where
  aux [] = []
  aux xs@(x:_) | length ys >= n = ys : aux zs
               | otherwise      = aux zs
    where (ys,zs) = span (x==) xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String -> [String]) -> Spec
specG llanuras = do
  it "e1" $
    llanuras 3 "aabbbcddddffxffxx" `shouldBe` ["bbb","dddd"]

spec :: Spec
spec = do
  describe "def. 1" $ specG llanuras1
  describe "def. 2" $ specG llanuras2
  describe "def. 3" $ specG llanuras3
  describe "def. 4" $ specG llanuras4
  describe "def. 5" $ specG llanuras5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Int -> [Int] -> Bool
prop_equivalencia n xs =
  all (== llanuras1 m xs)
      [llanuras2 m xs,
       llanuras3 m xs,
       llanuras4 m xs,
       llanuras5 m xs]
  where m = n `mod` 5

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (llanuras1 3 (take 2000000 (cycle "aaabcdeee")))
--    444444
--    (0.60 secs, 631,712,040 bytes)
--    λ> length (llanuras2 3 (take 2000000 (cycle "aaabcdeee")))
--    444444
--    (0.24 secs, 562,379,032 bytes)
--    λ> length (llanuras3 3 (take 2000000 (cycle "aaabcdeee")))
--    444444
--    (1.70 secs, 964,156,344 bytes)
--    λ> length (llanuras4 3 (take 2000000 (cycle "aaabcdeee")))
--    444444
--    (1.47 secs, 949,934,144 bytes)
--    λ> length (llanuras5 3 (take 2000000 (cycle "aaabcdeee")))
--    444444
--    (1.38 secs, 937,489,872 bytes)
