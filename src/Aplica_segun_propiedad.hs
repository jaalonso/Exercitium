-- Aplica_segun_propiedad.hs
-- Aplica según propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  ==  [5,6]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Aplica_segun_propiedad where

import Test.QuickCheck.HigherOrder (quickCheck')

-- 1ª solución
-- ===========

filtraAplica1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica1 f p xs = [f x | x <- xs, p x]

-- 2ª solución
-- ===========

filtraAplica2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica2 f p xs = map f (filter p xs)

-- 3ª solución
-- ===========

filtraAplica3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica3 _ _ [] = []
filtraAplica3 f p (x:xs) | p x       = f x : filtraAplica3 f p xs
                         | otherwise = filtraAplica3 f p xs

-- 4ª solución
-- ===========

filtraAplica4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica4 f p = foldr g []
  where g x y | p x       = f x : y
              | otherwise = y

-- 5ª solución
-- ===========

filtraAplica5 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica5 f p =
  foldr (\x y -> if p x then f x : y else y) []

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_filtraAplica :: (Int -> Int) -> (Int -> Bool) -> [Int] -> Bool
prop_filtraAplica f p xs =
  all (== filtraAplica1 f p xs)
      [filtraAplica2 f p xs,
       filtraAplica3 f p xs,
       filtraAplica4 f p xs,
       filtraAplica5 f p xs]

-- La comprobación es
--    λ> quickCheck' prop_filtraAplica
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sum (filtraAplica1 id even [1..5*10^6])
--    6250002500000
--    (2.92 secs, 1,644,678,696 bytes)
--    λ> sum (filtraAplica2 id even [1..5*10^6])
--    6250002500000
--    (1.17 secs, 1,463,662,848 bytes)
--    λ> sum (filtraAplica3 id even [1..5*10^6])
--    6250002500000
--    (3.18 secs, 1,964,678,640 bytes)
--    λ> sum (filtraAplica4 id even [1..5*10^6])
--    6250002500000
--    (2.64 secs, 1,924,678,752 bytes)
--    λ> sum (filtraAplica5 id even [1..5*10^6])
--    6250002500000
--    (2.61 secs, 1,824,678,712 bytes)
