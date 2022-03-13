-- Alfabeto_desde.hs
-- Alfabeto comenzando en un carácter.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    alfabetoDesde :: Char -> String
-- tal que (alfabetoDesde c) es el alfabeto, en minúscula, comenzando en
-- el carácter c, si c es una letra minúscula y comenzando en 'a', en
-- caso contrario. Por ejemplo,
--    alfabetoDesde 'e'  ==  "efghijklmnopqrstuvwxyzabcd"
--    alfabetoDesde 'a'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '7'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '{'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde 'B'  ==  "abcdefghijklmnopqrstuvwxyz"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Alfabeto_desde where

import Data.Char (isLower)
import Test.QuickCheck

-- 1ª solución
alfabetoDesde1 :: Char -> String
alfabetoDesde1 c =
  dropWhile (<c) ['a'..'z'] ++ takeWhile (<c) ['a'..'z']

-- 2ª solución
alfabetoDesde2 :: Char -> String
alfabetoDesde2 c = ys ++ xs
  where (xs,ys) = span (<c) ['a'..'z']

-- 3ª solución
alfabetoDesde3 :: Char -> String
alfabetoDesde3 c = ys ++ xs
  where (xs,ys) = break (==c) ['a'..'z']

-- 4ª solución
alfabetoDesde4 :: Char -> String
alfabetoDesde4 c
  | 'a' <= c && c <= 'z' = [c..'z'] ++ ['a'..pred c]
  | otherwise            = ['a'..'z']

-- 5ª solución
alfabetoDesde5 :: Char -> String
alfabetoDesde5 c
  | isLower c = [c..'z'] ++ ['a'..pred c]
  | otherwise = ['a'..'z']

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_alfabetoDesde :: Char -> Bool
prop_alfabetoDesde c =
  all (== alfabetoDesde1 c)
      [f c | f <- [alfabetoDesde2,
                   alfabetoDesde3,
                   alfabetoDesde4,
                   alfabetoDesde5]]

-- La comprobación es
