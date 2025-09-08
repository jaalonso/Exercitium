-- Empiezan_con_mayuscula.hs
-- Empiezan con mayúscula.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-Julio-2014 (actualizado 8-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir, por composición, la función
--    conMayuscula :: String -> Int
-- tal que (conMayuscula cs) es el número de palabras de cs que empiezan
-- con mayúscula. Por ejemplo.
--    conMayuscula "Juan vive en Sevilla o en Huelva"  ==  3
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Empiezan_con_mayuscula where

import Data.List (foldl')
import Data.Char (isUpper)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

conMayuscula1 :: String -> Int
conMayuscula1 cs =
  length [p | p <- words cs, isUpper (head p)]

-- 2ª solución
-- ===========

conMayuscula2 :: String -> Int
conMayuscula2 cs = length (filter empiezaConMayuscula (words cs))
  where
    empiezaConMayuscula p = isUpper (head p)

-- 3ª solución
-- ===========

conMayuscula3 :: String -> Int
conMayuscula3 cs = aux (words cs)
  where
    aux [] = 0
    aux (p:ps)
      | isUpper (head p) = 1 + aux ps
      | otherwise        = aux ps

-- 4ª solución
-- ===========

conMayuscula4 :: String -> Int
conMayuscula4 cs = foldr aux 0 (words cs)
  where
    aux p n
      | isUpper (head p) = 1 + n
      | otherwise        = n

-- 5ª solución
-- ===========

conMayuscula5 :: String -> Int
conMayuscula5 =
  foldr (\p n -> fromEnum (isUpper (head p)) + n) 0 . words

-- 6ª solución
-- ===========

conMayuscula6 :: String -> Int
conMayuscula6 =
  foldr ((+) . fromEnum . isUpper . head) 0 . words

-- 7ª solución
-- ===========

conMayuscula7 :: String -> Int
conMayuscula7 cs = foldl aux 0 (words cs)
  where
    aux n p
      | isUpper (head p) = n + 1
      | otherwise        = n

-- 8ª solución
-- ===========

conMayuscula8 :: String -> Int
conMayuscula8 cs = foldl' aux 0 (words cs)
  where
    aux n p
      | isUpper (head p) = n + 1
      | otherwise        = n

-- 9ª solución
-- ===========

conMayuscula9 :: String -> Int
conMayuscula9 =
  foldl' (\n p -> n + fromEnum (isUpper (head p))) 0 . words

-- 10ª solución
-- ============

conMayuscula10 :: String -> Int
conMayuscula10 =
  foldl' (flip ((+) . fromEnum . isUpper . head)) 0 . words

-- 11ª solución
-- ===========

conMayuscula11 :: String -> Int
conMayuscula11 =
  length . filter (isUpper . head) . words

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> Int) -> Spec
specG conMayuscula = do
  it "e1" $
    conMayuscula "Juan vive en Sevilla o en Huelva"  `shouldBe`  3

spec :: Spec
spec = do
  describe "def. 1"  $ specG conMayuscula1
  describe "def. 2"  $ specG conMayuscula2
  describe "def. 3"  $ specG conMayuscula3
  describe "def. 4"  $ specG conMayuscula4
  describe "def. 5"  $ specG conMayuscula5
  describe "def. 6"  $ specG conMayuscula6
  describe "def. 7"  $ specG conMayuscula7
  describe "def. 8"  $ specG conMayuscula8
  describe "def. 9"  $ specG conMayuscula9
  describe "def. 10"  $ specG conMayuscula10
  describe "def. 11"  $ specG conMayuscula11

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_conMayuscula :: String -> Bool
prop_conMayuscula cs =
  all (== conMayuscula1 cs)
      [conMayuscula2 cs,
       conMayuscula3 cs,
       conMayuscula4 cs,
       conMayuscula5 cs,
       conMayuscula6 cs,
       conMayuscula7 cs,
       conMayuscula8 cs,
       conMayuscula9 cs,
       conMayuscula10 cs,
       conMayuscula11 cs]

-- La comprobación es
--    λ> quickCheck prop_conMayuscula
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- (ejemplo n) es la cadena obtenida repitiendo n veces la cadena
-- "Hoy es Lunes". Por ejemplo
--    λ> ejemplo 3
--    "Hoy es Lunes Hoy es Lunes Hoy es Lunes"
ejemplo :: Int -> String
ejemplo n =
  unwords (concat (replicate n ["Hoy", "es", "Lunes"]))

-- La comparación es
--    λ> conMayuscula1 (ejemplo 1000000)
--    2000000
--    (1.62 secs, 3,080,601,344 bytes)
--    λ> conMayuscula2 (ejemplo 1000000)
--    2000000
--    (1.12 secs, 2,952,601,456 bytes)
--    λ> conMayuscula3 (ejemplo 1000000)
--    2000000
--    (2.39 secs, 3,489,575,816 bytes)
--    λ> conMayuscula4 (ejemplo 1000000)
--    2000000
--    (2.13 secs, 3,464,592,760 bytes)
--    λ> conMayuscula5 (ejemplo 1000000)
--    2000000
--    (2.14 secs, 3,301,526,888 bytes)
--    λ> conMayuscula6 (ejemplo 1000000)
--    2000000
--    (1.08 secs, 3,324,347,536 bytes)
--    λ> conMayuscula7 (ejemplo 1000000)
--    2000000
--    (3.71 secs, 3,488,396,128 bytes)
--    λ> conMayuscula8 (ejemplo 1000000)
--    2000000
--    (1.77 secs, 3,296,601,480 bytes)
--    λ> conMayuscula9 (ejemplo 1000000)
--    2000000
--    (1.76 secs, 3,104,601,400 bytes)
--    λ> conMayuscula10 (ejemplo 1000000)
--    2000000
--    (0.72 secs, 3,128,601,768 bytes)
--    λ> conMayuscula11 (ejemplo 1000000)
--    2000000
--    (0.61 secs, 2,904,601,664 bytes)
