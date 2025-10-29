-- Suma_de_todos_los_anteriores.hs
-- Suma de todos los anteriores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-Noviembre-2014 (actualizado 29-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaAnteriores :: [Integer] -> Bool
-- tal que (sumaAnteriores xs) se verifica si cada elemento de la lista
-- xs (excepto el primero) es la suma de sus anteriores elementos en la
-- lista. Por ejemplo,
--    sumaAnteriores [3,3,6,12]  ==  True
--    sumaAnteriores [3,3,7,10]  ==  False
--    sumaAnteriores [3]         ==  True
--    sumaAnteriores []          ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Suma_de_todos_los_anteriores where

import Data.List (isPrefixOf)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaAnteriores1 :: [Integer] -> Bool
sumaAnteriores1 xs = aux (reverse xs)
  where aux []     = True
        aux [_]    = True
        aux (y:ys) = y == sum ys && aux ys

-- 2ª solución
-- ===========

sumaAnteriores2 :: [Integer] -> Bool
sumaAnteriores2 (x:y:zs) =
  x == y && and [b == 2*a | (a,b) <- adyacentes (y:zs)]
sumaAnteriores2 _ = True

-- (adyacentes xs) es la lista de los elementos adyacentes de xs. Por
-- ejemplo,
--    adyacentes [3,3,6,12]  ==  [(3,3),(3,6),(6,12)]
adyacentes :: [a] -> [(a,a)]
adyacentes xs = zip xs (tail xs)

-- 3ª solución
-- ===========

sumaAnteriores3 :: [Integer] -> Bool
sumaAnteriores3 (x:xs) =
  xs == [x*2^n | n <- [0.. length xs - 1]]
sumaAnteriores3 _ = True

-- 4ª solución
-- ===========

sumaAnteriores4 :: [Integer] -> Bool
sumaAnteriores4 (x:xs) =
  xs `isPrefixOf` [x*2^n | n <- [0..]]
sumaAnteriores4 _ = True

-- 5ª solución
-- ===========

sumaAnteriores5 :: [Integer] -> Bool
sumaAnteriores5 [] = True
sumaAnteriores5 [_] = True
sumaAnteriores5 (x:xs) = aux x xs
  where
    aux _ [] = True
    aux z (y:ys) = y == z && aux (z + y) ys

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Integer] -> Bool) -> Spec
specG sumaAnteriores = do
  it "e1" $
    sumaAnteriores [3,3,6,12]  `shouldBe`  True
  it "e2" $
    sumaAnteriores [3,3,7,10]  `shouldBe`  False
  it "e3" $
    sumaAnteriores [3]         `shouldBe`  True
  it "e4" $
    sumaAnteriores []          `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaAnteriores1
  describe "def. 2" $ specG sumaAnteriores2
  describe "def. 3" $ specG sumaAnteriores3
  describe "def. 4" $ specG sumaAnteriores4
  describe "def. 5" $ specG sumaAnteriores5

-- La verificación es
--    λ> verifica
--    20 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sumaAnteriores :: [Integer] -> Bool
prop_sumaAnteriores xs =
  all (== sumaAnteriores1 xs)
      [sumaAnteriores2 xs,
       sumaAnteriores3 xs,
       sumaAnteriores4 xs,
       sumaAnteriores5 xs]

-- La comprobación es
--    λ> quickCheck prop_sumaAnteriores
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sumaAnteriores1 (3:[3*2^n | n <- [0..10000]])
--    True
--    (5.17 secs, 43,954,899,000 bytes)
--    λ> sumaAnteriores2 (3:[3*2^n | n <- [0..10000]])
--    True
--    (0.11 secs, 78,722,256 bytes)
--    λ> sumaAnteriores3 (3:[3*2^n | n <- [0..10000]])
--    True
--    (0.10 secs, 143,635,224 bytes)
--    λ> sumaAnteriores4 (3:[3*2^n | n <- [0..10000]])
--    True
--    (0.13 secs, 136,506,888 bytes)
--    λ> sumaAnteriores5 (3:[3*2^n | n <- [0..10000]])
--    True
--    (0.09 secs, 77,922,168 bytes)
--
--    λ> sumaAnteriores2 (3:[3*2^n | n <- [0..60000]])
--    True
--    (2.22 secs, 1,429,071,808 bytes)
--    λ> sumaAnteriores3 (3:[3*2^n | n <- [0..60000]])
--    True
--    (4.46 secs, 2,411,893,032 bytes)
--    λ> sumaAnteriores4 (3:[3*2^n | n <- [0..60000]])
--    True
--    (4.31 secs, 2,360,493,480 bytes)
--    λ> sumaAnteriores5 (3:[3*2^n | n <- [0..60000]])
--    True
--    (2.30 secs, 1,424,271,736 bytes)
