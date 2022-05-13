-- Numero_de_divisores.hs
-- Número de divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    numeroDivisores :: Integer -> Integer
-- tal que (numeroDivisores x) es el número de divisores de x. Por
-- ejemplo,
--    numeroDivisores 12  ==  6
--    numeroDivisores 25  ==  3
--    length (show (numeroDivisores (product [1..3*10^4])))  ==  1948
-- ---------------------------------------------------------------------

module Numero_de_divisores where

import Data.List (genericLength, group, inits)
import Data.Numbers.Primes (primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

numeroDivisores1 :: Integer -> Integer
numeroDivisores1 x =
  genericLength [y | y <- [1..x], x `mod` y == 0]

-- 2ª solución
-- ===========

numeroDivisores2 :: Integer -> Integer
numeroDivisores2 1 = 1
numeroDivisores2 x
  | esCuadrado x = 2 * genericLength [y | y <- [1..raizEntera x], x `mod` y == 0] - 1
  | otherwise    = 2 * genericLength [y | y <- [1..raizEntera x], x `mod` y == 0]

-- (raizEntera x) es el mayor número entero cuyo cuadrado es menor o
-- igual que x. Por ejemplo,
--    raizEntera 3  ==  1
--    raizEntera 4  ==  2
--    raizEntera 5  ==  2
--    raizEntera 8  ==  2
--    raizEntera 9  ==  3
raizEntera :: Integer -> Integer
raizEntera x = floor (sqrt (fromInteger x))

-- (esCuadrado x) se verifica si x es un cuadrado perfecto. Por ejemplo,
--    esCuadrado 9  ==  True
--    esCuadrado 7  ==  False
esCuadrado :: Integer -> Bool
esCuadrado x =
  x == (raizEntera x)^2

-- 3ª solución
-- ===========

numeroDivisores3 :: Integer -> Integer
numeroDivisores3 =
  genericLength . divisores

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 12  ==  [1,3,2,6,4,12]
--    divisores 25  ==  [1,5,25]
divisores :: Integer -> [Integer]
divisores = map (product . concat)
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

-- 4ª solución
-- ===========

numeroDivisores4 :: Integer -> Integer
numeroDivisores4 = genericLength
                 . map (product . concat)
                 . sequence
                 . map inits
                 . group
                 . primeFactors

-- 5ª solución
-- ===========

numeroDivisores5 :: Integer -> Integer
numeroDivisores5 =
  product . map ((+1) . genericLength) . group . primeFactors

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroDivisores :: Positive Integer -> Bool
prop_numeroDivisores (Positive x) =
  all (== numeroDivisores1 x)
      [ numeroDivisores2 x
      , numeroDivisores3 x
      , numeroDivisores4 x
      , numeroDivisores5 x]

-- La comprobación es
--    λ> quickCheck prop_numeroDivisores
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroDivisores1 (product [1..10])
--    270
--    (1.67 secs, 726,327,208 bytes)
--    λ> numeroDivisores2 (product [1..10])
--    270
--    (0.01 secs, 929,000 bytes)
--
--    λ> numeroDivisores2 (product [1..16])
--    5376
--    (2.10 secs, 915,864,664 bytes)
--    λ> numeroDivisores3 (product [1..16])
--    5376
--    (0.01 secs, 548,472 bytes)
--
--    λ> numeroDivisores3 (product [1..30])
--    2332800
--    (3.80 secs, 4,149,811,688 bytes)
--    λ> numeroDivisores4 (product [1..30])
--    2332800
--    (0.59 secs, 722,253,848 bytes)
--    λ> numeroDivisores5 (product [1..30])
--    2332800
--    (0.00 secs, 587,856 bytes)
