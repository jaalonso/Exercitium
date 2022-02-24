-- Anagramas.hs
-- Anagramas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una palabra es una anagrama de otra si se puede obtener permutando
-- sus letras. Por ejemplo, "mora" y "roma" son anagramas de "amor".
--
-- Definir la función
--    anagramas :: String -> [String] -> [String]
-- tal que (anagramas x ys) es la lista de los elementos de ys que son
-- anagramas de x. Por ejemplo,
--    λ> anagramas "amor" ["Roma","mola","loma","moRa", "rama"]
--    ["Roma","moRa"]
--    λ> anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"]
--    ["aMar","aRma"]
-- ---------------------------------------------------------------------

module Anagramas where

import Data.List (delete, permutations, sort)
import Data.Char (toLower)
import Data.Function (on)

-- 1ª solución
-- =============

anagramas :: String -> [String] -> [String]
anagramas _ [] = []
anagramas x (y:ys)
  | sonAnagramas x y = y : anagramas x ys
  | otherwise        = anagramas x ys

-- (sonAnagramas xs ys) se verifica si xs e ys son anagramas. Por
-- ejemplo,
--    sonAnagramas "amor" "Roma"  ==  True
--    sonAnagramas "amor" "mola"  ==  False
sonAnagramas :: String -> String -> Bool
sonAnagramas xs ys =
  sort (map toLower xs) == sort (map toLower ys)

-- 2ª solución
-- =============

anagramas2 :: String -> [String] -> [String]
anagramas2 _ [] = []
anagramas2 x (y:ys)
  | sonAnagramas2 x y = y : anagramas2 x ys
  | otherwise         = anagramas2 x ys

sonAnagramas2 :: String -> String -> Bool
sonAnagramas2 xs ys =
  (sort . map toLower) xs == (sort . map toLower) ys

-- 3ª solución
-- ===========

anagramas3 :: String -> [String] -> [String]
anagramas3 _ [] = []
anagramas3 x (y:ys)
  | sonAnagramas3 x y = y : anagramas3 x ys
  | otherwise         = anagramas3 x ys

sonAnagramas3 :: String -> String -> Bool
sonAnagramas3 = (==) `on` (sort . map toLower)

-- Nota. En la solución anterior se usa la función on ya que
--    (f `on` g) x y
-- es equivalente a
--    f (g x) (g y)
-- Por ejemplo,
--    λ> ((*) `on` (+2)) 3 4
--    30

-- 4ª solución
-- ===========

anagramas4 :: String -> [String] -> [String]
anagramas4 x ys = [y | y <- ys, sonAnagramas x y]

-- 5ª solución
-- ===========

anagramas5 :: String -> [String] -> [String]
anagramas5 x = filter (`sonAnagramas` x)

-- 6ª solución
-- ===========

anagramas6 :: String -> [String] -> [String]
anagramas6 x = filter (((==) `on` (sort . map toLower)) x)

-- 7ª solución
-- ===========

anagramas7 :: String -> [String] -> [String]
anagramas7 _ [] = []
anagramas7 x (y:ys)
  | sonAnagramas7 x y = y : anagramas7 x ys
  | otherwise         = anagramas7 x ys

sonAnagramas7 :: String -> String -> Bool
sonAnagramas7 xs ys = aux (map toLower xs) (map toLower ys)
  where
    aux [] [] = True
    aux [] _  = False
    aux (u:us) vs | u `notElem` vs = False
                  | otherwise      = aux us (delete u vs)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej = take (10^6) (permutations "1234567890")
--    λ> length (anagramas "1234567890" ej)
--    1000000
--    (2.27 secs, 5,627,236,104 bytes)
--    λ> length (anagramas2 "1234567890" ej)
--    1000000
--    (2.80 secs, 5,513,260,584 bytes)
--    λ> length (anagramas3 "1234567890" ej)
--    1000000
--    (1.86 secs, 5,097,260,856 bytes)
--    λ> length (anagramas4 "1234567890" ej)
--    1000000
--    (2.25 secs, 5,073,260,632 bytes)
--    λ> length (anagramas5 "1234567890" ej)
--    1000000
--    (2.14 secs, 5,009,260,616 bytes)
--    λ> length (anagramas6 "1234567890" ej)
--    1000000
--    (1.58 secs, 4,977,260,976 bytes)
--    λ> length (anagramas7 "1234567890" ej)
--    1000000
--    (6.63 secs, 6,904,821,648 bytes)
