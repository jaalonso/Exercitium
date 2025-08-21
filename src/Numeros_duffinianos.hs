-- Numeros_duffinianos.hs
-- Números duffinianos.
-- José A. Alonso Jiménez
-- Sevilla, 28 de abril de 2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números duffinianos https://bit.ly/2X1dMqd, llamados así por
-- Richard Duffy, son los números compuestos n que son coprimos con la
-- suma de sus divisores; es decir, n y la suma de los divisores de n no
-- tienen ningún factor primo común.
--
-- Por ejemplo, 35 es un número duffiniano ya que la suma de sus
-- divisores es 1 + 5 + 7 + 35 = 48 que es coprimo con 35.
--
-- Definir las funciones
--    esDuffiniano :: Integer -> Bool
--    duffinianos :: [Integer]
-- tales que
-- + (esDuffiniano n) se verifica si n es duffiniano. Por ejemplo,
--      esDuffiniano 35    ==  True
--      esDuffiniano 2021  ==  True
--      esDuffiniano 11    ==  False
--      esDuffiniano 12    ==  False
--      esDuffiniano (product [1..2*10^4])  ==  False
-- + duffinianos es la sucesión de los números duffinianos. Por ejemplo,
--      take 12 duffinianos  ==  [4,8,9,16,21,25,27,32,35,36,39,49]
--      length (takeWhile (<10^5) duffinianos)  ==  24434
--
-- Comprobar con QuickCheck que los números de la forma p^k, con p un
-- primo mayor que 2 y k un entero mayor que 1, son duffinianos.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_duffinianos where

import Data.List (foldl', genericLength, group, inits)
import Data.Numbers.Primes (isPrime, primeFactors, primes)
import Math.NumberTheory.ArithmeticFunctions (divisors, sigma)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Positive (Positive),
                        NonNegative (NonNegative),
                        quickCheck)

-- 1ª solución
-- ===========

duffinianos :: [Integer]
duffinianos = filter esDuffiniano1 [1..]

esDuffiniano1 :: Integer -> Bool
esDuffiniano1 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores1 n) == 1

-- (sumaDivisores n) es la suma de los divisores de n. Por ejemplo.
--    sumaDivisores 35  ==  48
sumaDivisores1 :: Integer -> Integer
sumaDivisores1 = sum . divisores

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 35  ==  [1,5,7,35]
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n]
                 , n `mod` x == 0]

-- 2ª solución
-- ===========

duffinianos2 :: [Integer]
duffinianos2 = filter esDuffiniano2 [1..]

esDuffiniano2 :: Integer -> Bool
esDuffiniano2 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores2 n) == 1

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sum . divisors

-- 3ª solución
-- ===========

duffinianos3 :: [Integer]
duffinianos3 = filter esDuffiniano3 [1..]

esDuffiniano3 :: Integer -> Bool
esDuffiniano3 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores3 n) == 1

sumaDivisores3 :: Integer -> Integer
sumaDivisores3 = foldl' (+) 0 . divisors

-- 4ª solución
-- ===========

duffinianos4 :: [Integer]
duffinianos4 = filter esDuffiniano4 [1..]

esDuffiniano4 :: Integer -> Bool
esDuffiniano4 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores4 n) == 1

sumaDivisores4 :: Integer -> Integer
sumaDivisores4 n = aux [1..n]
  where aux [] = 0
        aux (x:xs) | n `rem` x == 0 = x + aux xs
                   | otherwise      = aux xs

-- 5ª solución
-- ===========

duffinianos5 :: [Integer]
duffinianos5 = filter esDuffiniano5 [1..]

esDuffiniano5 :: Integer -> Bool
esDuffiniano5 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores5 n) == 1

sumaDivisores5 :: Integer -> Integer
sumaDivisores5 = sum
               . map (product . concat)
               . mapM inits
               . group
               . primeFactors

-- 6ª solución
-- ===========

duffinianos6 :: [Integer]
duffinianos6 = filter esDuffiniano6 [1..]

esDuffiniano6 :: Integer -> Bool
esDuffiniano6 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores6 n) == 1

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

-- (sumaDivisores6 n) es la suma de los divisores de n. Por ejemplo.
--      sumaDivisores6 35  ==  48
sumaDivisores6 :: Integer -> Integer
sumaDivisores6 x =
  product [(p^(e+1)-1) `div` (p-1) | (p,e) <- factorizacion x]

-- (factorizacion x) es la lista de las bases y exponentes de la
-- descomposición prima de x. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion = map primeroYlongitud . group . primeFactors

-- (primeroYlongitud xs) es el par formado por el primer elemento de xs
-- y la longitud de xs. Por ejemplo,
--    primeroYlongitud [3,2,5,7] == (3,4)
primeroYlongitud :: [a] -> (a,Integer)
primeroYlongitud (x:xs) = (x, 1 + genericLength xs)
primeroYlongitud _      = error "No tiene elementos"

-- 7ª solución
-- ===========

duffinianos7 :: [Integer]
duffinianos7 = filter esDuffiniano7 [1..]

esDuffiniano7 :: Integer -> Bool
esDuffiniano7 n =
  n > 1 &&
  not (isPrime n) &&
  gcd n (sumaDivisores7 n) == 1

sumaDivisores7 :: Integer -> Integer
sumaDivisores7 = sigma 1

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Bool) -> Spec
specG esDuffiniano = do
  it "e1" $
    esDuffiniano 35    `shouldBe`  True
  it "e2" $
    esDuffiniano 2021  `shouldBe`  True
  it "e3" $
    esDuffiniano 11    `shouldBe`  False
  it "e4" $
    esDuffiniano 12    `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG esDuffiniano1
  describe "def. 2" $ specG esDuffiniano2
  describe "def. 3" $ specG esDuffiniano3
  describe "def. 4" $ specG esDuffiniano4
  describe "def. 5" $ specG esDuffiniano5
  describe "def. 6" $ specG esDuffiniano6
  describe "def. 7" $ specG esDuffiniano7

-- La verificación es
--    λ> verifica
--
--    28 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_esDuffiniano :: Positive Integer -> Bool
prop_esDuffiniano (Positive n) =
  all (== esDuffiniano1 n)
      [f n | f <- [esDuffiniano2,
                   esDuffiniano3,
                   esDuffiniano4,
                   esDuffiniano5,
                   esDuffiniano6,
                   esDuffiniano7]]

-- La comprobación es
--    λ> quickCheck prop_esDuffiniano
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esDuffiniano1 (product [1..11])
--    False
--    (16.02 secs, 7,984,011,416 bytes)
--    λ> esDuffiniano2 (product [1..11])
--    False
--    (0.01 secs, 805,448 bytes)
--    λ> esDuffiniano3 (product [1..11])
--    False
--    (0.01 secs, 805,448 bytes)
--    λ> esDuffiniano4 (product [1..11])
--    False
--    (19.34 secs, 9,900,008,888 bytes)
--    λ> esDuffiniano5 (product [1..11])
--    False
--    (0.01 secs, 1,169,576 bytes)
--    λ> esDuffiniano6 (product [1..11])
--    False
--    (0.00 secs, 622,312 bytes)
--    λ> esDuffiniano7 (product [1..11])
--    False
--    (0.01 secs, 610,064 bytes)
--
--    λ> esDuffiniano2 (product [1..30])
--    False
--    (3.59 secs, 1,245,788,032 bytes)
--    λ> esDuffiniano3 (product [1..30])
--    False
--    (1.39 secs, 1,245,788,056 bytes)
--    λ> esDuffiniano5 (product [1..30])
--    False
--    (2.34 secs, 6,473,645,184 bytes)
--    λ> esDuffiniano6 (product [1..30])
--    False
--    (0.00 secs, 671,416 bytes)
--    λ> esDuffiniano7 (product [1..30])
--    False
--    (0.01 secs, 623,024 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_duffinianos :: NonNegative Int -> Positive Int -> Bool
prop_duffinianos (NonNegative n) (Positive k) =
  esDuffiniano7 (p^k')
  where p = primes !! n
        k' = k + 1

-- La comprobación es
--    λ> quickCheck prop_duffinianos
--    +++ OK, passed 100 tests.
