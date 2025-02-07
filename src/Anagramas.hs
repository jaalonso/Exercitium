-- Anagramas.hs
-- Anagramas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-febrero-2025
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Anagramas where

import Data.List (delete, sort, permutations)
import Data.Char (toLower)
import Data.Function (on)
import Data.Map (Map, fromListWith)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

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

-- 8ª solución
-- ===========

anagramas8 :: String -> [String] -> [String]
anagramas8 x = filter (`sonAnagramas8` x)

sonAnagramas8 :: String -> String -> Bool
sonAnagramas8 xs ys =
  frecuencias (map toLower xs) == frecuencias (map toLower ys)

frecuencias :: String -> Map Char Int
frecuencias xs = fromListWith (+) (zip xs (repeat 1))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> [String] -> [String]) -> Spec
specG anagramas' = do
  it "e1" $
    anagramas' "amor" ["Roma","mola","loma","moRa", "rama"] `shouldBe`
    ["Roma","moRa"]
  it "e2" $
    anagramas' "rama" ["aMar","amaRa","roMa","marr","aRma"] `shouldBe`
    ["aMar","aRma"]

spec :: Spec
spec = do
  describe "def. 1" $ specG anagramas
  describe "def. 2" $ specG anagramas2
  describe "def. 3" $ specG anagramas3
  describe "def. 4" $ specG anagramas4
  describe "def. 5" $ specG anagramas5
  describe "def. 6" $ specG anagramas6
  describe "def. 7" $ specG anagramas7
  describe "def. 8" $ specG anagramas8

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

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
--    λ> length (anagramas8 "1234567890" ej)
--    1000000
--    (3.90 secs, 9,679,323,632 bytes)

-- (cadena n) es la cadena de los 10 primeros caracteres de la cadena
-- infinita abcabcabc... Por ejemplo,
--    cadena 10  ==  "abcabcabca"
cadena :: Int -> String
cadena n = take n (cycle "abc")

-- (cadenas m n) es la lista otenida repitiendo m veces (adena n). Por
-- ejemplo,
--    cadenas 3 10 == ["abcabcabca","abcabcabca","abcabcabca"]
cadenas :: Int -> Int -> [String]
cadenas m n = replicate m (cadena n)

--    λ> length (anagramas6 (cadena (10^5)) (cadenas 100 (10^5)))
--    100
--    (8.42 secs, 23,126,067,704 bytes)
--    λ> length (anagramas8 (cadena (10^5)) (cadenas 100 (10^5)))
--    100
--    (4.06 secs, 7,220,705,856 bytes)
