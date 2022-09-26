-- Numeros_abundantes_impares.hs
-- Números abundantes impares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la lista
--   abundantesImpares :: [Integer]
-- cuyos elementos son los números abundantes impares. Por ejemplo,
--    λ> take 12 abundantesImpares3
--    [945,1575,2205,2835,3465,4095,4725,5355,5775,5985,6435,6615]
-- ---------------------------------------------------------------------

module Numeros_abundantes_impares where

import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.QuickCheck


-- 1ª solución
-- ===========

abundantesImpares1 :: [Integer]
abundantesImpares1 = [x | x <- [1,3..], numeroAbundante1 x]

-- (numeroAbundante n) se verifica si n es un número abundante. Por
-- ejemplo,
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
numeroAbundante1 :: Integer -> Bool
numeroAbundante1 x =
  x < sumaDivisores1 x - x

-- (sumaDivisores x) es la suma de los divisores de x. Por ejemplo,
--    sumaDivisores 12                 ==  28
--    sumaDivisores 25                 ==  31
sumaDivisores1 :: Integer -> Integer
sumaDivisores1 n = sum (divisores1 n)

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 60  ==  [1,5,3,15,2,10,6,30,4,20,12,60]
divisores1 :: Integer -> [Integer]
divisores1 n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª solución
-- ===========

abundantesImpares2 :: [Integer]
abundantesImpares2 = filter numeroAbundante1 [1,3..]

-- 3ª solución
-- ===========

-- Sustituyendo la definición de numeroAbundante1 de las soluciones
-- anteriores por cada una de las del ejercicio "Números abundantes"
-- https://bit.ly/3xSlWDU se obtiene una nueva definición de abundantes
-- impares. La usada en las definiciones anteriores es la menos
-- eficiente y la que se usa en la siguiente definición es la más eficiente.

abundantesImpares3 :: [Integer]
abundantesImpares3 = filter numeroAbundante3 [1,3..]

numeroAbundante3 :: Integer -> Bool
numeroAbundante3 x =
  x < sumaDivisores3 x - x

sumaDivisores3 :: Integer -> Integer
sumaDivisores3 = sigma 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_abundantesImpares :: Positive Int -> Bool
prop_abundantesImpares (Positive n) =
  all (== take n abundantesImpares1)
      [take n abundantesImpares2,
       take n abundantesImpares3]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_abundantesImpares
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> abundantesImpares1 !! 5
--    4095
--    (2.07 secs, 841,525,368 bytes)
--    λ> abundantesImpares2 !! 5
--    4095
--    (2.06 secs, 841,443,112 bytes)
--    λ> abundantesImpares3 !! 5
--    4095
--    (0.01 secs, 550,776 bytes)
