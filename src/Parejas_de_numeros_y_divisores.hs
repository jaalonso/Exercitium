-- Parejas_de_numeros_y_divisores.hs
-- Parejas de números y divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-Febrero-2015 (actualizado 12-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisoresHasta :: Int -> [(Int,Int)]
-- tal que (divisoresHasta n) es la lista de los pares (a,b) tales que a
-- es un número entre 2 y n y b es un divisor propio de a. Por ejemplo,
--    λ> divisoresHasta 6
--    [(2,1),(3,1),(4,1),(4,2),(5,1),(6,1),(6,2),(6,3)]
--    λ> divisoresHasta 8
--    [(2,1),(3,1),(4,1),(4,2),(5,1),(6,1),(6,2),(6,3),(7,1),(8,1),(8,2),(8,4)]
--    λ> length (divisoresHasta 1234567)
--    16272448
-- ---------------------------------------------------------------------

module Parejas_de_numeros_y_divisores where

import Data.List  (sort)
import Data.Array (accumArray, (!))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Enfoque directo por comprensión
-- ============================================

divisoresHasta1 :: Int -> [(Int,Int)]
divisoresHasta1 n =
  [(a,b) | a <- [2..n], b <- [1..a `div` 2], a `mod` b == 0]

-- 2ª solución: Enfoque funcional usando concatMap
-- ===============================================

divisoresHasta2 :: Int -> [(Int,Int)]
divisoresHasta2 n =
  concatMap (\a -> [(a,b) | b <- [1..a `div` 2], a `mod` b == 0]) [2..n]

-- 3ª solución: Enfoque de criba (generación de múltiplos) y ordenación
-- ====================================================================

divisoresHasta3 :: Int -> [(Int,Int)]
divisoresHasta3 n =
  sort [(a,b) | b <- [1..n `div` 2], a <- [2*b, 3*b..n]]

-- 4ª solución: Criba optimizada con Array
-- =======================================

divisoresHasta4 :: Int -> [(Int,Int)]
divisoresHasta4 n =
  [(a, b) | a <- [2..n], b <- tabla ! a]
  where
    tabla = accumArray (flip (:)) [] (2, n)
            [(k*b, b) | b <- [n `div` 2, n `div` 2 - 1 .. 1], k <- [2 .. n `div` b]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [(Int,Int)]) -> Spec
specG divisoresHasta = do
  it "e1" $
    divisoresHasta 6 `shouldBe`
    [(2,1),(3,1),(4,1),(4,2),(5,1),(6,1),(6,2),(6,3)]
  it "e2" $
    divisoresHasta 8 `shouldBe`
    [(2,1),(3,1),(4,1),(4,2),(5,1),(6,1),(6,2),(6,3),(7,1),(8,1),(8,2),(8,4)]

spec :: Spec
spec = do
  describe "def. 1" $ specG divisoresHasta1
  describe "def. 2" $ specG divisoresHasta2
  describe "def. 3" $ specG divisoresHasta3
  describe "def. 4" $ specG divisoresHasta4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== divisoresHasta1 n)
      [ divisoresHasta2 n
      , divisoresHasta3 n
      , divisoresHasta4 n
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (divisoresHasta1 4000)
--    (4000,2000)
--    (1.59 secs, 804,026,232 bytes)
--    λ> last (divisoresHasta2 4000)
--    (4000,2000)
--    (1.63 secs, 805,823,288 bytes)
--    λ> last (divisoresHasta3 4000)
--    (4000,2000)
--    (0.05 secs, 31,032,920 bytes)
--    λ> last (divisoresHasta4 4000)
--    (4000,2000)
--    (0.05 secs, 13,010,816 bytes)
--
--    λ> last (divisoresHasta3 100000)
--    (100000,50000)
--    (1.81 secs, 1,462,358,336 bytes)
--    λ> last (divisoresHasta4 100000)
--    (100000,50000)
--    (0.99 secs, 428,785,568 bytes)
