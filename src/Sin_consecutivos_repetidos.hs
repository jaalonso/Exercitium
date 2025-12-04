-- Sin_consecutivos_repetidos.hs
-- Sin consecutivos repetidos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-Diciembre-2014 (actualizado 4-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sinConsecutivosRepetidos :: Eq a => [a] -> [a]
-- tal que (sinConsecutivosRepetidos xs) es la lista obtenida a partir
-- de xs de forma que si hay dos o más elementos idénticos consecutivos,
-- borra las repeticiones y deja sólo el primer elemento. Por ejemplo,
--    λ> sinConsecutivosRepetidos "eesssooo essss   toodddooo"
--    "eso es todo"
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Sin_consecutivos_repetidos where

import Data.List (group)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sinConsecutivosRepetidos1 :: Eq a => [a] -> [a]
sinConsecutivosRepetidos1 [] = []
sinConsecutivosRepetidos1 [x] = [x]
sinConsecutivosRepetidos1 (x : y : xs)
  | x == y    = sinConsecutivosRepetidos1 (y : xs)
  | otherwise = x : sinConsecutivosRepetidos1 (y : xs)

-- 2ª solución
-- ===========

sinConsecutivosRepetidos2 :: Eq a => [a] -> [a]
sinConsecutivosRepetidos2 [] = []
sinConsecutivosRepetidos2 (x:xs) =
  x : sinConsecutivosRepetidos2 (dropWhile (==x) xs)

-- 3ª solución
-- ===========

sinConsecutivosRepetidos3 :: Eq a => [a] -> [a]
sinConsecutivosRepetidos3 xs = foldr aux [] xs
  where
    aux x [] = [x]
    aux x (y:ys)
      | x == y    = y : ys
      | otherwise = x : y : ys

-- 4ª solución
-- ===========

sinConsecutivosRepetidos4 :: Eq a => [a] -> [a]
sinConsecutivosRepetidos4 xs =
  [x | (x:_) <- group xs]


-- 5ª solución
-- ===========

sinConsecutivosRepetidos5 :: Eq a => [a] -> [a]
sinConsecutivosRepetidos5 = map head . group

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String) -> Spec
specG sinConsecutivosRepetidos = do
  it "e1" $
    sinConsecutivosRepetidos "eesssooo essss   toodddooo"
    `shouldBe` "eso es todo"

spec :: Spec
spec = do
  describe "def. 1" $ specG sinConsecutivosRepetidos1
  describe "def. 2" $ specG sinConsecutivosRepetidos2
  describe "def. 3" $ specG sinConsecutivosRepetidos3
  describe "def. 4" $ specG sinConsecutivosRepetidos4
  describe "def. 5" $ specG sinConsecutivosRepetidos5

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
  all (== sinConsecutivosRepetidos1 xs)
      [sinConsecutivosRepetidos2 xs,
       sinConsecutivosRepetidos3 xs,
       sinConsecutivosRepetidos4 xs,
       sinConsecutivosRepetidos5 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (sinConsecutivosRepetidos1 (take (5*10^6) (cycle "aaabb ")))
--    2500000
--    (2.15 secs, 1,420,601,960 bytes)
--    λ> length (sinConsecutivosRepetidos2 (take (5*10^6) (cycle "aaabb ")))
--    2500000
--    (1.54 secs, 1,080,602,240 bytes)
--    λ> length (sinConsecutivosRepetidos3 (take (5*10^6) (cycle "aaabb ")))
--    2500000
--    (2.94 secs, 1,575,252,040 bytes)
--    λ> length (sinConsecutivosRepetidos4 (take (5*10^6) (cycle "aaabb ")))
--    2500000
--    (0.86 secs, 1,360,602,312 bytes)
--    λ> length (sinConsecutivosRepetidos5 (take (5*10^6) (cycle "aaabb ")))
--    2500000
--    (0.29 secs, 1,400,602,512 bytes)
