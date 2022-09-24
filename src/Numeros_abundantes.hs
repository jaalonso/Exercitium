-- Numeros_abundantes.hs
-- Números abundantes.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número natural n se denomina [abundante](https://bit.ly/3Uk4XUE)
-- si es menor que la suma de sus divisores propios. Por ejemplo, 12 es
-- abundante ya que la suma de sus divisores propios es 16
-- (= 1 + 2 + 3 + 4 + 6), pero 5 y 28 no lo son.
--
-- Definir la función
--    numeroAbundante :: Int -> Bool
-- tal que (numeroAbundante n) se verifica si n es un número
-- abundante. Por ejemplo,
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
--    numeroAbundante 100000000  ==  True
--    numeroAbundante 100000001  ==  False
-- ---------------------------------------------------------------------

module Numeros_abundantes where

import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.QuickCheck

-- 1ª solución
-- ===========

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

-- Sustituyendo la definición de sumaDivisores de la solución anterior por
-- cada una de las del ejercicio [Suma de divisores](https://bit.ly/3S9aonQ)
-- se obtiene una nueva definición de numeroAbundante. La usada en la
-- definición anterior es la menos eficiente y la que se usa en la
-- siguiente definición es la más eficiente.

numeroAbundante2 :: Integer -> Bool
numeroAbundante2 x =
  x < sumaDivisores2 x - x

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sigma 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroAbundante :: Positive Integer -> Bool
prop_numeroAbundante (Positive n) =
  numeroAbundante1 n == numeroAbundante2 n

-- La comprobación es
--    λ> quickCheck prop_numeroAbundante
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> numeroAbundante1 (5*10^6)
--    True
--    (2.55 secs, 1,000,558,840 bytes)
--    λ> numeroAbundante2 (5*10^6)
--    True
--    (0.00 secs, 555,408 bytes)
