-- Precio_total.hs
-- Precio total.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-Noviembre-2014 (actualizado 1-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una función de precio determina el precio de cada elemento; por
-- ejemplo,
--    precioCI :: String -> Int
--    precioCI "leche"       = 10
--    precioCI "mantequilla" = 18
--    precioCI "patatas"     = 22
--    precioCI "chocolate"   = 16
--
-- Definir la función
--    precioTotal :: (String -> Int) -> [String] -> Int
-- tal que (precioTotal f xs) es el precio total de los elementos de xs
-- respecto de la función de precio f. Por ejemplo,
--    precioTotal precioCI ["leche", "leche", "mantequilla"]  ==  38
--    precioTotal precioCI ["chocolate", "mantequilla"]       ==  34
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Precio_total where

import Data.List (foldl')
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

precioCI :: String -> Int
precioCI "leche"       = 10
precioCI "mantequilla" = 18
precioCI "patatas"     = 22
precioCI "chocolate"   = 16

-- 1ª solución
-- ===========

precioTotal1 :: (String -> Int) -> [String] -> Int
precioTotal1 f xs = sum [f x | x <- xs]

-- 2ª solución
-- ===========

precioTotal2 :: (String -> Int) -> [String] -> Int
precioTotal2 f xs = sum (map f xs)

-- 3ª solución
-- ===========

precioTotal3 :: (String -> Int) -> [String] -> Int
precioTotal3 f = sum . map f

-- 4ª solución
-- ===========

precioTotal4 :: (String -> Int) -> [String] -> Int
precioTotal4 _ []     = 0
precioTotal4 f (x:xs) = f x + precioTotal4 f xs

-- 5ª solución
-- ===========

precioTotal5 :: (String -> Int) -> [String] -> Int
precioTotal5 f = foldr g 0
  where g x y = f x + y

-- 6ª solución
-- ===========

precioTotal6 :: (String -> Int) -> [String] -> Int
precioTotal6 f = foldr (\x y -> f x + y) 0

-- 7ª solución
-- ===========

precioTotal7 :: (String -> Int) -> [String] -> Int
precioTotal7 f = foldr ((+) . f) 0

-- 8ª solución
-- ===========

precioTotal8 :: (a -> Int) -> [a] -> Int
precioTotal8 f = foldl' (\r x -> r + f x) 0

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((String -> Int) -> [String] -> Int) -> Spec
specG precioTotal = do
  it "e1" $
    precioTotal precioCI ["leche", "leche", "mantequilla"]  `shouldBe`  38
  it "e2" $
    precioTotal precioCI ["chocolate", "mantequilla"]       `shouldBe`  34

spec :: Spec
spec = do
  describe "def. 1" $ specG precioTotal1
  describe "def. 2" $ specG precioTotal2
  describe "def. 3" $ specG precioTotal3
  describe "def. 4" $ specG precioTotal4
  describe "def. 5" $ specG precioTotal5
  describe "def. 6" $ specG precioTotal6
  describe "def. 7" $ specG precioTotal7
  describe "def. 8" $ specG precioTotal8

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> precioTotal1 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (2.50 secs, 2,064,602,672 bytes)
--    λ> precioTotal2 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (1.70 secs, 1,968,602,576 bytes)
--    λ> precioTotal3 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (1.69 secs, 1,968,602,704 bytes)
--    λ> precioTotal4 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (3.47 secs, 2,312,057,376 bytes)
--    λ> precioTotal5 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (2.67 secs, 2,045,822,944 bytes)
--    λ> precioTotal6 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (2.65 secs, 2,045,822,904 bytes)
--    λ> precioTotal7 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (2.19 secs, 2,093,822,984 bytes)
--    λ> precioTotal8 precioCI (replicate (3*10^6) "leche")
--    30000000
--    (2.20 secs, 1,848,602,480 bytes)
