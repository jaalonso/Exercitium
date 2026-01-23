-- Numeros_polidivisibles.hs
-- Números polidivisibles.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-Enero-2015 (actualizado 20-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número natural es polidivisible si cumple las siguientes
-- condiciones:
-- + El número formado por sus dos primeros dígitos es divisible por 2.
-- + El número formado por sus tres primeros dígitos es divisible por 3.
-- + El número formado por sus cuatros primeros dígitos es divisible por 4.
-- + etcétera.
--
-- Por ejemplo, el número 345654 es un número polidivisible ya que
-- + 34     es divisible por 2,
-- + 345    es divisible por 3,
-- + 3456   es divisible por 4,
-- + 34565  es divisible por 5 y
-- + 345654 es divisible por 6.
-- pero 123456 no lo es, porque 1234 no es divisible por 4.
--
-- Definir las funciones
--   polidivisibles :: [Integer]
--   polidivisiblesN :: Integer -> [Integer]
-- tales que
-- + polidivisible es la sucesión cuyos elementos son los números
--   polidivisibles. Por ejemplo,
--      λ> take 20 polidivisibles
--      [1,2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,24,26,28,30]
--      λ> take 10 (dropWhile (<100) polidivisibles)
--      [102,105,108,120,123,126,129,141,144,147]
--      λ> polidivisibles !! 20455
--      3608528850368400786036725
-- + (polidivisiblesN k) es la lista de los números polidivisibles con k
--   dígitos. Por ejemplo,
--      λ> polidivisiblesN 2
--      [10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,
--       50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,
--       90,92,94,96,98]
--      λ> polidivisiblesN 25
--      [3608528850368400786036725]
--      (0.01 secs, 615,984 bytes)
--      λ> polidivisiblesN 26
--      []
--
-- Comprobar que, para n entre 1 y 5, la cantidad de números
-- polidivisibles de n dígitos es 9*10^(n-1)/n!.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_polidivisibles where

import Control.Applicative ((<$>))
import Data.List (inits, tails, sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

polidivisibles1 :: [Integer]
polidivisibles1 = [n | n <- [1..], esPolidivisible1 n]

-- (esPolidivisible n) se verifica si n es polidivisible. Por ejemplo,
--    esPolidivisible 345654  ==  True
--    esPolidivisible 123456  ==  False
esPolidivisible1 :: Integer -> Bool
esPolidivisible1 n =
  and [(n `div` 10^(m-k)) `mod` k == 0 | k <- [2..m]]
  where m = fromIntegral (length (show n))

polidivisiblesN1 :: Integer -> [Integer]
polidivisiblesN1 n =
  takeWhile (<=10^n-1) (dropWhile (<10^(n-1)) polidivisibles1)

-- 2ª solución
-- ===========

polidivisibles2 :: [Integer]
polidivisibles2 = filter esPolidivisible2 [1..]

esPolidivisible2 :: Integer -> Bool
esPolidivisible2 n = d == 1 || n `mod` d == 0 && nxt
  where d   = nDigitos n
        nxt = esPolidivisible2 $ n `div` 10

-- (nDigitos n) es el número de dígitos de n.
nDigitos :: Integer -> Integer
nDigitos 0 = 0
nDigitos n = 1 + nDigitos (n `div` 10)

polidivisiblesN2 :: Integer -> [Integer]
polidivisiblesN2 k =
  takeWhile ((k==) . nDigitos) . dropWhile ((k>) . nDigitos) $ polidivisibles2

-- 3ª solución
-- ===========

polidivisibles3 :: [Integer]
polidivisibles3 = concatMap polidivisiblesN3 [1..]

polidivisiblesN3 :: Integer -> [Integer]
polidivisiblesN3 k = filter (esPolidivisible3 k) [10^(k-1)..10^k-1]

esPolidivisible3 :: Integer -> Integer -> Bool
esPolidivisible3 d n = d == 1 || n `mod` d == 0 && nxt
   where nxt = esPolidivisible3 (d-1) $ n `div` 10

-- 4ª solución
-- ===========

polidivisibles4 :: [Integer]
polidivisibles4 = concatMap polidivisiblesN4 [1..]

polidivisiblesN4 :: Integer -> [Integer]
polidivisiblesN4 1 = [1..9]
polidivisiblesN4 k =
  [n * 10 + d | n <- polidivisiblesN4 (k - 1),
                d <- [0..9],
                (n * 10 + d) `mod` k == 0]

-- 5ª solución
-- ===========

polidivisibles5 :: [Integer]
polidivisibles5 =
  concat (takeWhile (not . null) (map polidivisiblesN5 [1..]))

polidivisiblesN5 :: Integer -> [Integer]
polidivisiblesN5 = (map p [0..] !!) . fromIntegral
  where p 0 = []
        p 1 = [1..9]
        p n = [r | q <- polidivisiblesN5 (n - 1),
                   d <- [0..9],
                   let r = 10 * q + d,
                   r `mod` n == 0]

-- 6ª solución
-- ===========

polidivisibles6 :: [Integer]
polidivisibles6 = concat polidivisiblesPorNivel

-- polidivisiblesPorNivel es lalista de listas, donde el elemento
-- i-ésimo contiene los polidivisibles de (i+1) dígitos. Por ejemplo,
--    λ> take 4 (map (take 3) polidivisiblesPorNivel)
--    [[1,2,3],[10,12,14],[102,105,106],[1020,1024,1026]]
polidivisiblesPorNivel :: [[Integer]]
polidivisiblesPorNivel = scanl siguienteNivel [1..9] [2..]
  where
    siguienteNivel anteriores k =
      [n * 10 + d | n <- anteriores,
                    d <- [0..9],
                    (n * 10 + d) `rem` k == 0]

polidivisiblesN6 :: Integer -> [Integer]
polidivisiblesN6 0 = []
polidivisiblesN6 k = polidivisiblesPorNivel !! (fromIntegral k - 1)

-- 7ª solución
-- ===========

polidivisibles7 :: [Integer]
polidivisibles7 = concatMap polidivisiblesN7 [1..]

polidivisiblesN7 :: Integer -> [Integer]
polidivisiblesN7 1 = [1..9]
polidivisiblesN7 k = concatMap f (polidivisiblesN7 (k-1))
  where f n = [idiv,idiv+k..i+1*10-1]
          where i    = n * 10
                idiv = i + ((k - i `mod` k) `mod` k)

-- 8ª solución
-- ===========

polidivisibles8 :: [Integer]
polidivisibles8 = concat todosPolidivisiblesN

todosPolidivisiblesN :: [[Integer]]
todosPolidivisiblesN = snd <$> iterate f (1,[1..9])
  where f (k,ps) = (k+1, ps >>= g (k+1))
        g k n = [idiv,idiv+k..i+1*10-1]
          where i    = n * 10
                idiv = i + ((k - i `mod` k) `mod` k)

polidivisiblesN8 :: Integer -> [Integer]
polidivisiblesN8 k = todosPolidivisiblesN !! (fromInteger k-1)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: [Integer] -> Spec
specG1 polidivisibles = do
  it "e1" $
    take 20 polidivisibles `shouldBe`
    [1,2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,24,26,28,30]

specG2 :: (Integer -> [Integer]) -> Spec
specG2 polidivisiblesN = do
  it "e1" $
    polidivisiblesN 2 `shouldBe`
    [10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,
     50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,
     90,92,94,96,98]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 polidivisibles1
  describe "def. 2" $ specG1 polidivisibles2
  describe "def. 3" $ specG1 polidivisibles3
  describe "def. 4" $ specG1 polidivisibles4
  describe "def. 5" $ specG1 polidivisibles5
  describe "def. 6" $ specG1 polidivisibles6
  describe "def. 7" $ specG1 polidivisibles7
  describe "def. 8" $ specG1 polidivisibles8
  describe "def. 1" $ specG2 polidivisiblesN1
  describe "def. 2" $ specG2 polidivisiblesN2
  describe "def. 3" $ specG2 polidivisiblesN3
  describe "def. 4" $ specG2 polidivisiblesN4
  describe "def. 5" $ specG2 polidivisiblesN5
  describe "def. 6" $ specG2 polidivisiblesN6
  describe "def. 7" $ specG2 polidivisiblesN7
  describe "def. 8" $ specG2 polidivisiblesN8

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Bool
prop_equivalencia =
  all (== polidivisibles1 !! 100)
      [polidivisibles2 !! 100,
       polidivisibles3 !! 100,
       polidivisibles4 !! 100,
       polidivisibles5 !! 100,
       polidivisibles6 !! 100,
       polidivisibles7 !! 100,
       polidivisibles8 !! 100] &&
  all (== polidivisiblesN1 5)
      [polidivisiblesN2 5,
       polidivisiblesN3 5,
       polidivisiblesN4 5,
       polidivisiblesN5 5,
       polidivisiblesN6 5,
       polidivisiblesN7 5,
       polidivisiblesN8 5]

-- La comprobación es
--    λ> prop_equivalencia
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> polidivisibles1 !! 2000
--    600450
--    (1.73 secs, 1,721,437,936 bytes)
--    λ> polidivisibles2 !! 2000
--    600450
--    (2.45 secs, 1,183,164,888 bytes)
--    λ> polidivisibles3 !! 2000
--    600450
--    (0.56 secs, 325,839,536 bytes)
--    λ> polidivisibles4 !! 2000
--    600450
--    (0.05 secs, 7,017,768 bytes)
--    λ> polidivisibles5 !! 2000
--    600450
--    (0.04 secs, 4,310,608 bytes)
--    λ> polidivisibles6 !! 2000
--    600450
--    (0.02 secs, 6,186,712 bytes)
--    λ> polidivisibles7 !! 2000
--    600450
--    (0.02 secs, 2,265,064 bytes)
--    λ> polidivisibles8 !! 2000
--    600450
--    (0.02 secs, 2,047,080 bytes)
--
--    λ> polidivisibles4 !! 20400
--    66325288502466081020
--    (1.24 secs, 742,363,096 bytes)
--    λ> polidivisibles5 !! 20400
--    66325288502466081020
--    (0.18 secs, 73,575,992 bytes)
--    λ> polidivisibles6 !! 20400
--    66325288502466081020
--    (0.22 secs, 110,739,664 bytes)
--    λ> polidivisibles7 !! 20400
--    66325288502466081020
--    (0.25 secs, 161,849,000 bytes)
--    λ> polidivisibles8 !! 20400
--    66325288502466081020
--    (0.07 secs, 26,334,328 bytes)
--
--    λ> length (polidivisiblesN1 6)
--    1200
--    (2.67 secs, 2,963,892,336 bytes)
--    λ> length (polidivisiblesN2 6)
--    1200
--    (3.94 secs, 2,029,126,896 bytes)
--    λ> length (polidivisiblesN3 6)
--    1200
--    (0.73 secs, 481,891,328 bytes)
--    λ> length (polidivisiblesN4 6)
--    1200
--    (0.02 secs, 5,816,336 bytes)
--    λ> length (polidivisiblesN5 6)
--    1200
--    (0.04 secs, 5,376,216 bytes)
--    λ> length (polidivisiblesN6 6)
--    1200
--    (0.03 secs, 7,868,920 bytes)
--    λ> length (polidivisiblesN7 6)
--    1200
--    (0.01 secs, 1,834,744 bytes)
--    λ> length (polidivisiblesN8 6)
--    1200
--    (0.02 secs, 2,358,456 bytes)
--
--    λ> length (polidivisiblesN3 7)
--    1713
--    (6.73 secs, 4,617,303,360 bytes)
--    λ> length (polidivisiblesN4 7)
--    1713
--    (0.05 secs, 10,463,304 bytes)
--    λ> length (polidivisiblesN5 7)
--    1713
--    (0.03 secs, 9,647,272 bytes)
--    λ> length (polidivisiblesN6 7)
--    1713
--    (0.03 secs, 14,363,544 bytes)
--    λ> length (polidivisiblesN7 7)
--    1713
--    (0.01 secs, 2,888,104 bytes)
--    λ> length (polidivisiblesN8 7)
--    1713
--    (0.01 secs, 3,882,768 bytes)

-- Propiedad
-- =========

-- (conjetura k) se verifica si la cantidad de números polidivisibles de
-- k dígitos es 9*10^(k-1)/k!.
conjetura :: Integer -> Bool
conjetura k =
  fromIntegral (length (polidivisiblesN1 k)) == 9*10^(k-1) `div` factorial k
  where factorial n = product [1..n]

-- La comprobación de la conjetura para k entre 1 y 5 es
--    λ> all conjetura [1..5]
--    True
