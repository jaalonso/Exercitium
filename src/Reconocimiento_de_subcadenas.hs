-- Reconocimiento_de_subcadenas.hs
-- Reconocimiento de subcadenas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir, por recursión, la función
--    esSubcadena :: String -> String -> Bool
-- tal que (esSubcadena xs ys) se verifica si xs es una subcadena de
-- ys. Por ejemplo,
--    esSubcadena "casa" "escasamente"   ==  True
--    esSubcadena "cante" "escasamente"  ==  False
--    esSubcadena "" ""                  ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Reconocimiento_de_subcadenas where

import Data.List (isPrefixOf, isInfixOf, tails)
import Test.QuickCheck

-- 1ª solución
-- ===========

esSubcadena1 :: String -> String -> Bool
esSubcadena1 [] _      = True
esSubcadena1  _ []     = False
esSubcadena1 xs (y:ys) = xs `isPrefixOf` (y:ys) || xs `esSubcadena1` ys

-- 2ª solución
-- ===========

esSubcadena2 :: String -> String -> Bool
esSubcadena2 xs ys =
  or [xs `isPrefixOf` zs | zs <- sufijos ys]

-- (sufijos xs) es la lista de sufijos de xs. Por ejemplo,
--    sufijos "abc"  ==  ["abc","bc","c",""]
sufijos :: String -> [String]
sufijos xs = [drop i xs | i <- [0..length xs]]

-- 3ª solución
-- ===========

esSubcadena3 :: String -> String -> Bool
esSubcadena3 xs ys =
  or [xs `isPrefixOf` zs | zs <- tails ys]

-- 4ª solución
-- ===========

esSubcadena4 :: String -> String -> Bool
esSubcadena4 xs ys =
  any (xs `isPrefixOf`) (tails ys)

-- 5ª solución
-- ===========

esSubcadena5 :: String -> String -> Bool
esSubcadena5 = (. tails) . any . isPrefixOf

-- 6ª solución
-- ===========

esSubcadena6 :: String -> String -> Bool
esSubcadena6 = isInfixOf

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_esSubcadena :: String -> String -> Bool
prop_esSubcadena xs ys =
  all (== esSubcadena1 xs ys)
      [esSubcadena2 xs ys,
       esSubcadena3 xs ys,
       esSubcadena4 xs ys,
       esSubcadena5 xs ys,
       esSubcadena6 xs ys]

-- La comprobación es
--    λ> quickCheck prop_esSubcadena
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esSubcadena1 "abc" (replicate (5*10^4) 'd' ++ "abc")
--    True
--    (0.03 secs, 17,789,392 bytes)
--    λ> esSubcadena2 "abc" (replicate (5*10^4) 'd' ++ "abc")
--    True
--    (6.32 secs, 24,989,912 bytes)
--
--    λ> esSubcadena1 "abc" (replicate (5*10^6) 'd' ++ "abc")
--    True
--    (3.24 secs, 1,720,589,432 bytes)
--    λ> esSubcadena3 "abc" (replicate (5*10^6) 'd' ++ "abc")
--    True
--    (1.81 secs, 1,720,589,656 bytes)
--    λ> esSubcadena4 "abc" (replicate (5*10^6) 'd' ++ "abc")
--    True
--    (0.71 secs, 1,120,589,480 bytes)
--    λ> esSubcadena5 "abc" (replicate (5*10^6) 'd' ++ "abc")
--    True
--    (0.41 secs, 1,120,589,584 bytes)
--    λ> esSubcadena6 "abc" (replicate (5*10^6) 'd' ++ "abc")
--    True
--    (0.11 secs, 560,589,200 bytes)
