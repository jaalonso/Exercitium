-- Repeticion_de_elementos.hs
-- Repetición de elementos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-Noviembre-2014 (actualizado 5-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    repiteElementos :: Int -> [a] -> [a]
-- tal que (repiteElementos k xs) es la lista obtenida repitiendo cada
-- elemento de xs k veces. Por ejemplo,
--    repiteElementos 3 [5,2,7,4]  ==  [5,5,5,2,2,2,7,7,7,4,4,4]
--
-- Comprobar con QuickCheck que, para todo número natural k y toda lista
-- xs, el número de elementos de (repiteElementos k xs) es k veces el
-- número de elementos de xs.
-- ---------------------------------------------------------------------

module Repeticion_de_elementos where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

repiteElementos1 :: Int -> [a] -> [a]
repiteElementos1 k xs = concat [replicate k x | x <- xs]

-- 2ª solución
-- ===========

repiteElementos2 :: Int -> [a] -> [a]
repiteElementos2 k xs = concat (map (replicate k) xs)

-- 3ª solución
-- ===========

repiteElementos3 :: Int -> [a] -> [a]
repiteElementos3 k = concatMap (replicate k)

-- 4ª solución
-- ===========

repiteElementos4 :: Int -> [a] -> [a]
repiteElementos4 _ [] = []
repiteElementos4 k (x:xs) = replicate k x ++ repiteElementos4 k xs

-- 5ª solución
-- ===========

repiteElementos5 :: Int -> [a] -> [a]
repiteElementos5 k = foldr ((++) . replicate k) []

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> [Int]) -> Spec
specG repiteElementos = do
  it "e1" $
    repiteElementos 3 [5,2,7,4] `shouldBe` [5,5,5,2,2,2,7,7,7,4,4,4]

spec :: Spec
spec = do
  describe "def. 1" $ specG repiteElementos1
  describe "def. 2" $ specG repiteElementos2
  describe "def. 3" $ specG repiteElementos3
  describe "def. 4" $ specG repiteElementos4
  describe "def. 5" $ specG repiteElementos5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Int -> [Int] -> Bool
prop_equivalencia k xs =
  repiteElementos2 k xs == ys &&
  repiteElementos3 k xs == ys &&
  repiteElementos4 k xs == ys &&
  repiteElementos5 k xs == ys
  where ys = repiteElementos1 k xs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (repiteElementos1 10000 [1..10000])
--    100000000
--    (1.28 secs, 11,202,762,144 bytes)
--    λ> length (repiteElementos2 10000 [1..10000])
--    100000000
--    (1.28 secs, 11,202,442,120 bytes)
--    λ> length (repiteElementos3 10000 [1..10000])
--    100000000
--    (1.26 secs, 11,201,562,128 bytes)
--    λ> length (repiteElementos4 10000 [1..10000])
--    100000000
--    (1.30 secs, 11,202,762,024 bytes)
--    λ> length (repiteElementos5 10000 [1..10000])
--    100000000
--    (1.33 secs, 11,202,202,208 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_repiteElementos :: NonNegative Int -> [Int] -> Bool
prop_repiteElementos (NonNegative k) xs =
  length (repiteElementos1 k xs) == k * length xs

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_repiteElementos
--    +++ OK, passed 100 tests.
