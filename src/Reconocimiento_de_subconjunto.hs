-- Reconocimiento_de_subconjunto.hs
-- Reconocimiento de subconjunto.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- ys. por ejemplo,
--    subconjunto [3,2,3] [2,5,3,5]  ==  True
--    subconjunto [3,2,3] [2,5,6,5]  ==  False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Reconocimiento_de_subconjunto where

import Test.QuickCheck

-- 1ª definición
subconjunto1 :: Eq a => [a] -> [a] -> Bool
subconjunto1 xs ys =
  [x | x <- xs, x `elem` ys] == xs

-- 2ª definición
subconjunto2 :: Eq a => [a] -> [a] -> Bool
subconjunto2 []     _  = True
subconjunto2 (x:xs) ys = x `elem` ys && subconjunto2 xs ys

-- 3ª definición
subconjunto3 :: Eq a => [a] -> [a] -> Bool
subconjunto3 xs ys =
  all (`elem` ys) xs

-- 4ª definición
subconjunto4 :: Eq a => [a] -> [a] -> Bool
subconjunto4 []       _                   = True
subconjunto4 _        []                  = False
subconjunto4 zs@(x:xs) (y:ys) | x == y    = subconjunto4 xs ys
                              | otherwise = subconjunto4 zs ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_subconjunto :: [Int] -> [Int] -> Bool
prop_subconjunto xs ys =
  all (== subconjunto1 xs ys)
      [subconjunto2 xs ys,
       subconjunto3 xs ys,
       subconjunto4 xs ys]

-- La comprobación es
--    λ> quickCheck prop_subconjunto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> subconjunto1 [1..2*10^4] [1..2*10^4]
--    True
--    (1.81 secs, 5,992,448 bytes)
--    λ> subconjunto2 [1..2*10^4] [1..2*10^4]
--    True
--    (1.83 secs, 6,952,200 bytes)
--    λ> subconjunto3 [1..2*10^4] [1..2*10^4]
--    True
--    (1.75 secs, 4,712,304 bytes)
--    λ> subconjunto4 [1..2*10^4] [1..2*10^4]
--    True
--    (0.05 secs, 6,152,224 bytes)
