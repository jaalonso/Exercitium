-- Numeros_perfectos.hs
-- Números perfectos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un números entero positivo es [perfecto](https://bit.ly/3BIN0be) si
-- es igual a la suma de sus divisores, excluyendo el propio número. Por
-- ejemplo, 6 es un número perfecto porque sus divisores propios son 1,
-- 2 y 3; y 6 = 1 + 2 + 3.
--
-- Definir la función
--    perfectos :: Integer -> [Integer]
-- tal que (perfectos n) es la lista de todos los números perfectos
-- menores que n. Por ejemplo,
--    perfectos 500     ==  [6,28,496]
--    perfectos (10^5)  ==  [6,28,496,8128]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Numeros_perfectos where

import Data.List (genericLength, group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

perfectos1 :: Integer -> [Integer]
perfectos1 n =
  [x | x <- [1..n],
       sum (init (divisores1 x)) == x]

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
divisores1 :: Integer -> [Integer]
divisores1 n =
  [x | x <- [1..n],
       n `mod` x == 0]

-- 2ª solución
-- ===========

perfectos2 :: Integer -> [Integer]
perfectos2 n =
  [x | x <- [1..n],
       sum (init (divisores2 x)) == x]

divisores2 :: Integer -> [Integer]
divisores2 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores2 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

-- (primerosDivisores n) es la lista de los divisores del número n cuyo
-- cuadrado es menor o gual que n. Por ejemplo,
--    primerosDivisores 25  ==  [1,5]
--    primerosDivisores 30  ==  [1,2,3,5]
primerosDivisores2 :: Integer -> [Integer]
primerosDivisores2 n =
   [x | x <- [1..round (sqrt (fromIntegral n))],
        n `mod` x == 0]

-- 3ª solución
-- ===========

perfectos3 :: Integer -> [Integer]
perfectos3 n =
  [x | x <- [1..n],
       sum (init (divisores3 x)) == x]

divisores3 :: Integer -> [Integer]
divisores3 n = xs ++ [n `div` y | y <- ys]
  where xs = primerosDivisores3 n
        (z:zs) = reverse xs
        ys | z^2 == n  = zs
           | otherwise = z:zs

primerosDivisores3 :: Integer -> [Integer]
primerosDivisores3 n =
   filter ((== 0) . mod n) [1..round (sqrt (fromIntegral n))]

-- 4ª solución
-- ===========

perfectos4 :: Integer -> [Integer]
perfectos4 n =
  [x | x <- [1..n],
       sum (init (divisores4 x)) == x]

divisores4 :: Integer -> [Integer]
divisores4 =
  nub . sort . map product . subsequences . primeFactors

-- 5ª solución
-- ===========

perfectos5 :: Integer -> [Integer]
perfectos5 n =
  [x | x <- [1..n],
       sum (init (divisores5 x)) == x]

divisores5 :: Integer -> [Integer]
divisores5 =
  sort
  . map (product . concat)
  . productoCartesiano
  . map inits
  . group
  . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 6ª solución
-- ===========

perfectos6 :: Integer -> [Integer]
perfectos6 n =
  [x | x <- [1..n],
       sum (init (divisores6 x)) == x]

divisores6 :: Integer -> [Integer]
divisores6 = sort
           . map (product . concat)
           . mapM inits
           . group
           . primeFactors

-- 7ª solución
-- ===========

perfectos7 :: Integer -> [Integer]
perfectos7 n =
  [x | x <- [1..n],
       sumaDivisores x == 2 * x]

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

sumaDivisores :: Integer -> Integer
sumaDivisores x =
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
primeroYlongitud (x:xs) =
  (x, 1 + genericLength xs)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_perfectos :: Positive Integer -> Bool
prop_perfectos (Positive n) =
  all (== perfectos1 n)
      [perfectos2 n,
       perfectos3 n,
       perfectos4 n,
       perfectos5 n,
       perfectos6 n,
       perfectos7 n]

-- La comprobación es
--    λ> quickCheck prop_perfectos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (perfectos1 (4*10^3))
--    496
--    (3.88 secs, 1,608,259,072 bytes)
--    λ> last (perfectos2 (4*10^3))
--    496
--    (0.12 secs, 50,194,408 bytes)
--    λ> last (perfectos3 (4*10^3))
--    496
--    (0.06 secs, 37,069,704 bytes)
--    λ> last (perfectos4 (4*10^3))
--    496
--    (0.08 secs, 123,635,864 bytes)
--    λ> last (perfectos5 (4*10^3))
--    496
--    (0.14 secs, 118,811,560 bytes)
--    λ> last (perfectos6 (4*10^3))
--    496
--    (0.05 secs, 102,155,192 bytes)
--    λ> last (perfectos7 (4*10^3))
--    496
--    (0.09 secs, 75,252,688 bytes)
--
--    λ> last (perfectos2 (6*10^4))
--    8128
--    (4.64 secs, 2,240,164,520 bytes)
--    λ> last (perfectos3 (6*10^4))
--    8128
--    (1.95 secs, 1,460,563,992 bytes)
--    λ> last (perfectos4 (6*10^4))
--    8128
--    (1.42 secs, 4,025,328,664 bytes)
--    λ> last (perfectos5 (6*10^4))
--    8128
--    (1.96 secs, 3,338,449,560 bytes)
--    λ> last (perfectos6 (6*10^4))
--    8128
--    (0.99 secs, 2,955,292,392 bytes)
--    λ> last (perfectos7 (6*10^4))
--    8128
--    (1.04 secs, 2,342,047,784 bytes)
