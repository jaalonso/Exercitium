-- Sumas_de_4_primos.hs
-- Sumas de 4 primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La [conjetura de Waring sobre los números primos](http://bit.ly/1L19MIT)
-- establece que todo número impar es primo o la suma de tres primos. La
-- [conjetura de Goldbach](http://bit.ly/1enFuR8) afirma que todo número
-- par mayor que 2 es la suma de dos números primos. Ambos problemas
-- ha estado abiertos durante más de 200 años. En este problema no se
-- propone su solución, sino una tarea más simple: buscar una manera de
-- expresar los enteros mayores que 7 como suma de exactamente cuatro
-- números primos; es decir, definir la función
--    suma4primos :: Integer -> [(Integer,Integer,Integer,Integer)]
-- tal que (suma4primos n) es la lista de las cuádruplas crecientes
-- (a,b,c,d) de números primos cuya suma es n (que se supone mayor que
-- 7). Por ejemplo,
--    suma4primos 18             == [(2,2,3,11),(2,2,7,7),(3,3,5,7),(3,5,5,5)]
--    head (suma4primos (10^14)) == (2,2,23,99999999999973)
--
-- Comprobar con QuickCheck que todo entero mayor que 7 se puede
-- escribir como suma de exactamente cuatro números primos.
-- ---------------------------------------------------------------------

module Sumas_de_4_primos where

import Data.Numbers.Primes (isPrime, primes)
import Test.QuickCheck

-- 1ª solución
-- ===========

suma4primos1 :: Integer -> [(Integer, Integer, Integer, Integer)]
suma4primos1 n =
  [(a,b,c,d) | let as = takeWhile (< n) primes,
               a <- as,
               let bs = takeWhile (< n-a) as,
               b <- bs, a <= b,
               let cs = takeWhile (< n-a-b) bs,
               c <- cs, b <= c,
               let d = n-a-b-c, c <= d,
               isPrime d]

-- 2ª solución
-- ===========

suma4primos2 :: Integer -> [(Integer, Integer, Integer, Integer)]
suma4primos2 n =
  [(a,b,c,d) | let as = takeWhile (< n) primes,
               a <- as,
               let bs = takeWhile (< n-a) (dropWhile (< a) as),
               b <- bs,
               let cs = takeWhile (<n-a-b) (dropWhile (< b) bs),
               c <- cs,
               let d = n-a-b-c,
               c <= d,
               isPrime d]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_suma4primos :: Positive Integer -> Bool
prop_suma4primos (Positive n) =
  suma4primos1 n == suma4primos2 n

-- La comprobación es
--    λ> quickCheck prop_suma4primos
--    +++ OK, passed 100 tests; 526 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (suma4primos1 2000)
--    90219
--    (2.98 secs, 4,517,620,744 bytes)
--    λ> length (suma4primos2 2000)
--    90219
--    (2.22 secs, 4,223,251,928 bytes)
--
--    λ> head (suma4primos1 (10^14))
--    (2,2,23,99999999999973)
--    (1.67 secs, 5,963,327,168 bytes)
--    λ> head (suma4primos2 (10^14))
--    (2,2,23,99999999999973)
--    (1.70 secs, 5,963,326,848 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_suma4primos2 :: Integer -> Property
prop_suma4primos2 n =
  n > 7 ==> not (null (suma4primos1 n))

-- La comprobación es
--    λ> quickCheck prop_suma4primos2
--    +++ OK, passed 100 tests; 582 discarded.
