-- Numeros_abundantes_menores_o_iguales_que_n.hs
-- Números abundantes menores o iguales que n.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número natural n se denomina [abundante](https://bit.ly/3Uk4XUE)
-- si es menor que la suma de sus divisores propios. Por ejemplo, 12 es
-- abundante ya que la suma de sus divisores propios es 16
-- (= 1 + 2 + 3 + 4 + 6), pero 5 y 28 no lo son.
--
-- Definir la función
--    numerosAbundantesMenores :: Integer -> [Integer]
-- tal que (numerosAbundantesMenores n) es la lista de números
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
--    numerosAbundantesMenores 48  ==  [12,18,20,24,30,36,40,42,48]
--    length (numerosAbundantesMenores (10^6)) ==  247545
-- ---------------------------------------------------------------------

module Numeros_abundantes_menores_o_iguales_que_n where

import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.QuickCheck

-- 1ª solución
-- ===========

numerosAbundantesMenores1 :: Integer -> [Integer]
numerosAbundantesMenores1 n =
  [x | x <- [1..n],
      numeroAbundante1 x]

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

-- Sustituyendo la definición de numeroAbundante de la solución anterior por
-- cada una de las del ejercicio [Números abundantes](https://bit.ly/3xSlWDU)
-- se obtiene una nueva definición de numerosAbundantesMenores. La usada en la
-- definición anterior es la menos eficiente y la que se usa en la
-- siguiente definición es la más eficiente.

numerosAbundantesMenores2 :: Integer -> [Integer]
numerosAbundantesMenores2 n =
  [x | x <- [1..n],
      numeroAbundante2 x]

numeroAbundante2 :: Integer -> Bool
numeroAbundante2 x =
  x < sumaDivisores2 x - x

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sigma 1

-- 3ª solución
-- ===========

numerosAbundantesMenores3 :: Integer -> [Integer]
numerosAbundantesMenores3 n =
  filter numeroAbundante2 [1..n]

-- 4ª solución
-- ===========

numerosAbundantesMenores4 :: Integer -> [Integer]
numerosAbundantesMenores4 =
  filter numeroAbundante2 . enumFromTo 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numerosAbundantesMenores :: Positive Integer -> Bool
prop_numerosAbundantesMenores (Positive n) =
  all (== numerosAbundantesMenores1 n)
      [numerosAbundantesMenores2 n,
       numerosAbundantesMenores3 n,
       numerosAbundantesMenores4 n]

-- La comprobación es
--    λ> quickCheck prop_numerosAbundantesMenores
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (numerosAbundantesMenores1 (5*10^3))
--    1239
--    (5.49 secs, 2,508,692,808 bytes)
--    λ> length (numerosAbundantesMenores2 (5*10^3))
--    1239
--    (0.01 secs, 11,501,944 bytes)

--    λ> length (numerosAbundantesMenores2 (10^6))
--    247545
--    (1.48 secs, 2,543,048,024 bytes)
--    λ> length (numerosAbundantesMenores3 (10^6))
--    247545
--    (1.30 secs, 2,499,087,272 bytes)
--    λ> length (numerosAbundantesMenores4 (10^6))
--    247545
--    (1.30 secs, 2,499,087,248 bytes)
