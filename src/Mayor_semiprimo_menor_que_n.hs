-- Mayor_semiprimo_menor_que_n.hs
-- Mayor semiprimo menor que n.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un [número semiprimo](http://bit.ly/1NK8bJ0) es un número natural que
-- es producto de dos números primos no necesariamente distintos. Por
-- ejemplo, 26 es semiprimo (porque 26 = 2*13) y 49 también lo es
-- (porque 49 = 7*7). 

-- Definir la función
--    mayorSemiprimoMenor :: Integer -> Integer
-- tal que (mayorSemiprimoMenor n) es el mayor semiprimo menor que
-- n (suponiendo que n > 4). Por ejemplo,
--    mayorSemiprimoMenor 27      ==  26
--    mayorSemiprimoMenor 50      ==  49
--    mayorSemiprimoMenor 49      ==  46
--    mayorSemiprimoMenor (10^15) == 999999999999998
-- ---------------------------------------------------------------------

module Mayor_semiprimo_menor_que_n where

import Data.Numbers.Primes (primeFactors, isPrime, primes)
import Test.QuickCheck

-- 1ª solución
-- ===========

mayorSemiprimoMenor1 :: Integer -> Integer
mayorSemiprimoMenor1 n =
  head [x | x <- [n-1,n-2..2], semiPrimo x]

semiPrimo :: Integer -> Bool
semiPrimo n =
  not (null [x | x <- [n,n-1..2], 
                 primo x,
                 n `mod` x == 0,
                 primo (n `div` x)])

primo :: Integer -> Bool
primo n = [x | x <- [1..n], n `mod` x == 0] == [1,n] 

-- 2ª solución
-- ===========

mayorSemiprimoMenor2 :: Integer -> Integer
mayorSemiprimoMenor2 n =
  head [x | x <- [n-1,n-2..2], semiPrimo2 x]

semiPrimo2 :: Integer -> Bool
semiPrimo2 n =
  not (null [x | x <- [n-1,n-2..2], 
                 isPrime x,
                 n `mod` x == 0,
                 isPrime (n `div` x)])

-- 3ª solución
-- ===========

mayorSemiprimoMenor3 :: Integer -> Integer
mayorSemiprimoMenor3 n =
  head [x | x <- [n-1,n-2..2], semiPrimo3 x]

semiPrimo3 :: Integer -> Bool
semiPrimo3 n =
  not (null [x | x <- reverse (takeWhile (<n) primes),
                 n `mod` x == 0,
                 isPrime (n `div` x)])

-- 4ª solución
-- ===========

mayorSemiprimoMenor4 :: Integer -> Integer
mayorSemiprimoMenor4 n =
  head [ p | p <- [n-1,n-2..2]
           , (length . primeFactors) p == 2]

-- 5ª solución
-- ===========

mayorSemiprimoMenor5 :: Integer -> Integer
mayorSemiprimoMenor5 n
  | semiPrimo5 (n-1) = n-1
  | otherwise        = mayorSemiprimoMenor5 (n-1)

semiPrimo5 :: Integer -> Bool
semiPrimo5 x = length (primeFactors x) == 2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mayorSemiprimoMenor :: Integer -> Property
prop_mayorSemiprimoMenor n =
  n > 4 ==>
  all (== mayorSemiprimoMenor1 n)
      [mayorSemiprimoMenor2 n,
       mayorSemiprimoMenor3 n,
       mayorSemiprimoMenor4 n,
       mayorSemiprimoMenor5 n]

-- La comprobación es
--    λ> quickCheck prop_mayorSemiprimoMenor
--    +++ OK, passed 100 tests; 353 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mayorSemiprimoMenor1 5000
--    4997
--    (1.92 secs, 945,507,880 bytes)
--    λ> mayorSemiprimoMenor2 5000
--    4997
--    (0.05 secs, 123,031,264 bytes)
--    λ> mayorSemiprimoMenor3 5000
--    4997
--    (0.01 secs, 5,865,120 bytes)
--    λ> mayorSemiprimoMenor4 5000
--    4997
--    (0.00 secs, 593,528 bytes)
--    λ> mayorSemiprimoMenor5 5000
--    4997
--    (0.00 secs, 593,200 bytes)
--
--    λ> mayorSemiprimoMenor3 (3*10^6)
--    2999995
--    (2.34 secs, 6,713,620,000 bytes)
--    λ> mayorSemiprimoMenor4 (2*10^6)
--    1999997
--    (0.01 secs, 728,936 bytes)
--    λ> mayorSemiprimoMenor5 (2*10^6)
--    1999997
--    (0.01 secs, 728,608 bytes)
