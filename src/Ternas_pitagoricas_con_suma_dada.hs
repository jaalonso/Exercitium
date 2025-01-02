-- Ternas_pitagoricas_con_suma_dada.hs
-- Ternas pitagóricas con suma dada
-- José A. Alonso Jiménez
-- Sevilla, 10-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una terna pitagórica es una terna de números naturales (a,b,c) tal
-- que a<b<c y a^2+b^2=c^2. Por ejemplo (3,4,5) es una terna pitagórica.
--
-- Definir la función
--    ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
-- tal que (ternasPitagoricas x) es la lista de las ternas pitagóricas
-- cuya suma es x. Por ejemplo,
--    ternasPitagoricas 12     == [(3,4,5)]
--    ternasPitagoricas 60     == [(10,24,26),(15,20,25)]
--    ternasPitagoricas (10^6) == [(218750,360000,421250),(200000,375000,425000)]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ternas_pitagoricas_con_suma_dada where

import Data.List (nub,sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

ternasPitagoricas1 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas1 x =
  [(a,b,c) | a <- [0..x],
             b <- [a+1..x],
             c <- [b+1..x],
             a^2 + b^2 == c^2,
             a+b+c == x]

-- 2ª solución
-- ===========

ternasPitagoricas2 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas2 x =
  [(a,b,c) | a <- [1..x],
             b <- [a+1..x-a],
             let c = x-a-b,
             a^2+b^2 == c^2]

-- 3ª solución
-- ===========

-- Todas las ternas pitagóricas primitivas (a,b,c) pueden representarse
-- por
--    a = m^2 - n^2, b = 2*m*n, c = m^2 + n^2,
-- con 1 <= n < m. (Ver en https://tinyurl.com/27ydumhz ).

ternasPitagoricas3 :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas3 x =
  nub [(d*a,d*b,d*c) | d <- [1..x],
                       x `mod` d == 0,
                       (a,b,c) <- aux (x `div` d)]
  where
    aux y = [(a,b,c) | m <- [2..limite],
                       n <- [1..m-1],
                       let [a,b] = sort [m^2 - n^2, 2*m*n],
                       let c = m^2 + n^2,
                       a+b+c == y]
      where limite = ceiling (sqrt (fromIntegral y))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [(Integer,Integer,Integer)]) -> Spec
specG ternasPitagoricas = do
  it "e1" $
    ternasPitagoricas 12      `shouldBe` [(3,4,5)]
  it "e2" $
    ternasPitagoricas 60      `shouldBe` [(10,24,26),(15,20,25)]

spec :: Spec
spec = do
  describe "def. 1" $ specG ternasPitagoricas1
  describe "def. 2" $ specG ternasPitagoricas2
  describe "def. 3" $ specG ternasPitagoricas3

-- La verificación es
--    λ> verifica
--
--    6 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ternasPitagoricas :: Positive Integer -> Bool
prop_ternasPitagoricas (Positive x) =
  all (== ternasPitagoricas1 x)
      [ternasPitagoricas2 x,
       ternasPitagoricas3 x]

-- La comprobación es
--    λ> quickCheck prop_ternasPitagoricas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ternasPitagoricas1 200
--    [(40,75,85)]
--    (1.90 secs, 2,404,800,856 bytes)
--    λ> ternasPitagoricas2 200
--    [(40,75,85)]
--    (0.06 secs, 19,334,232 bytes)
--    λ> ternasPitagoricas3 200
--    [(40,75,85)]
--    (0.01 secs, 994,224 bytes)
--
--    λ> ternasPitagoricas2 3000
--    [(500,1200,1300),(600,1125,1275),(750,1000,1250)]
--    (4.41 secs, 4,354,148,136 bytes)
--    λ> ternasPitagoricas3 3000
--    [(500,1200,1300),(600,1125,1275),(750,1000,1250)]
--    (0.05 secs, 17,110,360 bytes)
