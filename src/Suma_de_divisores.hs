-- Suma_de_divisores.hs
-- Suma de divisores.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaDivisores :: Integer -> Integer
-- tal que (sumaDivisores x) es la suma de los divisores de x. Por ejemplo,
--    sumaDivisores 12  ==  28
--    sumaDivisores 25  ==  31
--    sumaDivisores (product [1..25])  ==  93383273455325195473152000
--    length (show (sumaDivisores (product [1..30000])))  ==  121289
--    maximum (map sumaDivisores [1..10^5])  ==  403200
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Suma_de_divisores where

import Data.List (genericLength, group, inits)
import Data.Numbers.Primes (primeFactors)
import Test.QuickCheck

-- 1ª solución
-- ===========

sumaDivisores1 :: Integer -> Integer
sumaDivisores1 = sum . divisores

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 60  ==  [1,5,3,15,2,10,6,30,4,20,12,60]
divisores :: Integer -> [Integer]
divisores = map (product . concat)
          . productoCartesiano
          . map inits
          . group
          . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 2ª solución
-- ===========

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sum
               . map (product . concat)
               . sequence
               . map inits
               . group
               . primeFactors

-- 3ª solución
-- ===========

-- Si la descomposición de x en factores primos es
--    x = p(1)^e(1) . p(2)^e(2) . .... . p(n)^e(n)
-- entonces la suma de los divisores de x es
--    p(1)^(e(1)+1) - 1     p(2)^(e(2)+1) - 1       p(n)^(e(2)+1) - 1
--   ------------------- . ------------------- ... -------------------
--        p(1)-1                p(2)-1                  p(n)-1
-- Ver la demostración en http://bit.ly/2zUXZPc

sumaDivisores3 :: Integer -> Integer
sumaDivisores3 x =
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
prop_sumaDivisores :: Positive Integer -> Bool
prop_sumaDivisores (Positive x) =
  all (== sumaDivisores1 x)
      [ sumaDivisores2 x
      , sumaDivisores3 x
      ]

-- La comprobación es
--    λ> quickCheck prop_sumaDivisores
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> sumaDivisores1 251888923423315469521109880000000
--   1471072204661054993275791673480320
--   (10.63 secs, 10,614,618,080 bytes)
--   λ> sumaDivisores2 251888923423315469521109880000000
--   1471072204661054993275791673480320
--   (2.51 secs, 5,719,399,056 bytes)
--   λ> sumaDivisores3 251888923423315469521109880000000
--   1471072204661054993275791673480320
--   (0.01 secs, 177,480 bytes)
