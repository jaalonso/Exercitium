-- Divisibles_por_el_primero.hs
-- Divisibles por el primero.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-Octubre-2014 (actualizado 20-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisiblesPorPrimero :: [Int] -> Bool
-- tal que (divisibles xs) se verifica si todos los elementos positivos
-- de xs son divisibles por el primero. Por ejemplo,
--    divisiblesPorPrimero [2,6,-3,0,18,-17,10]  ==  True
--    divisiblesPorPrimero [-13]                 ==  True
--    divisiblesPorPrimero [-3,6,1,-3,9,18]      ==  False
--    divisiblesPorPrimero [5,-2,-6,3]           ==  False
--    divisiblesPorPrimero []                    ==  True
--    divisiblesPorPrimero [0,2,4]               ==  False
--    divisiblesPorPrimero []                    ==  True
--    divisiblesPorPrimero [0,-2,-4]             ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Divisibles_por_el_primero where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisiblesPorPrimero1 :: [Int] -> Bool
divisiblesPorPrimero1 []     = True
divisiblesPorPrimero1 (0:xs) = null (positivos xs)
divisiblesPorPrimero1 (x:xs) = and [y `rem` x == 0 | y <- positivos xs]

-- (positivos xs) es la lista de los elementos positivos de xs. Por
-- ejemplo,
--    positivos [2,6,-3,0,18,-17,10] == [2,6,18,10]
positivos :: [Int] -> [Int]
positivos xs = [y | y <- xs, y > 0]

-- 2ª solución
-- ===========

divisiblesPorPrimero2 :: [Int] -> Bool
divisiblesPorPrimero2 []     = True
divisiblesPorPrimero2 (0:xs) = null (positivos xs)
divisiblesPorPrimero2 (x:xs) = aux xs
  where aux [] = True
        aux (y:ys) | y > 0     = y `rem` x == 0 && aux ys
                   | otherwise = aux ys

-- 3ª solución
-- ===========

divisiblesPorPrimero3 :: [Int] -> Bool
divisiblesPorPrimero3 []     = True
divisiblesPorPrimero3 (0:xs) = all (<=0) xs
divisiblesPorPrimero3 (x:xs) = all (\y -> y `rem` x == 0) (filter (>0) xs)

-- 4ª solución
-- ===========

divisiblesPorPrimero4 :: [Int] -> Bool
divisiblesPorPrimero4 []     = True
divisiblesPorPrimero4 (0:xs) = all (<=0) xs
divisiblesPorPrimero4 (x:xs) = all (\y -> y <= 0 || y `rem` x == 0) xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> Bool) -> Spec
specG divisiblesPorPrimero = do
  it "e1" $
    divisiblesPorPrimero [2,6,-3,0,18,-17,10] `shouldBe` True
  it "e2" $
    divisiblesPorPrimero [-13]                `shouldBe` True
  it "e3" $
    divisiblesPorPrimero [-3,6,1,-3,9,18]     `shouldBe` False
  it "e4" $
    divisiblesPorPrimero [5,-2,-6,3]          `shouldBe` False
  it "e5" $
    divisiblesPorPrimero []                   `shouldBe` True
  it "e6" $
    divisiblesPorPrimero [0,2,4]              `shouldBe` False
  it "e7" $
    divisiblesPorPrimero [0,-2,-4]            `shouldBe` True

spec :: Spec
spec = do
  describe "def. 1" $ specG divisiblesPorPrimero1
  describe "def. 2" $ specG divisiblesPorPrimero2
  describe "def. 3" $ specG divisiblesPorPrimero3
  describe "def. 4" $ specG divisiblesPorPrimero4

-- La verificación es
--    λ> verifica
--    28 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_divisiblesPorPrimero :: [Int] -> Bool
prop_divisiblesPorPrimero xs =
  all (== divisiblesPorPrimero1 xs)
      [divisiblesPorPrimero2 xs,
       divisiblesPorPrimero3 xs,
       divisiblesPorPrimero4 xs]

-- La comprobación es
--    λ> quickCheck prop_divisiblesPorPrimero
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> divisiblesPorPrimero1 [2,4..6*10^6]
--    True
--    (2.10 secs, 1,536,598,784 bytes)
--    λ> divisiblesPorPrimero2 [2,4..6*10^6]
--    True
--    (2.18 secs, 1,200,598,688 bytes)
--    λ> divisiblesPorPrimero3 [2,4..6*10^6]
--    True
--    (1.15 secs, 936,598,936 bytes)
--    λ> divisiblesPorPrimero4 [2,4..6*10^6]
--    True
--    (1.66 secs, 1,104,598,744 bytes)
