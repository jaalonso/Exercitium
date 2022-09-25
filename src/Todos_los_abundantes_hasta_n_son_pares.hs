-- Todos_los_abundantes_hasta_n_son_pares.hs
-- Todos los abundantes hasta n son pares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    todosPares :: Integer -> Bool
-- tal que (todosPares n) se verifica si todos los números abundantes
-- menores o iguales que n son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

module Todos_los_abundantes_hasta_n_son_pares where

import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.QuickCheck

-- 1ª solución
-- ===========

todosPares1 :: Integer -> Bool
todosPares1 n = and [even x | x <- numerosAbundantesMenores1 n]

-- (numerosAbundantesMenores n) es la lista de números abundantes
-- menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
--    numerosAbundantesMenores 48  ==  [12,18,20,24,30,36,40,42,48]
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

-- Sustituyendo la definición de numerosAbundantesMenores de la solución
-- anterior por cada una de las del ejercicio anterior se obtiene una
-- nueva definición de todosPares. La usada en la definición anterior es
-- la menos eficiente y la que se usa en la siguiente definición es la
-- más eficiente.

todosPares2 :: Integer -> Bool
todosPares2 n = and [even x | x <- numerosAbundantesMenores2 n]

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

todosPares3 :: Integer -> Bool
todosPares3 1 = True
todosPares3 n | numeroAbundante1 n = even n && todosPares3 (n-1)
              | otherwise          = todosPares3 (n-1)

-- 4ª solución
-- ===========

todosPares4 :: Integer -> Bool
todosPares4 n = all even (numerosAbundantesMenores1 n)

-- 5ª solución
-- ===========

todosPares5 :: Integer -> Bool
todosPares5 = all even . numerosAbundantesMenores1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_todosPares :: Positive Integer -> Bool
prop_todosPares (Positive n) =
  all (== todosPares1 n)
      [todosPares2 n,
       todosPares3 n,
       todosPares4 n,
       todosPares5 n]

-- La comprobación es
--    λ> quickCheck prop_todosPares
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> todosPares1 (10^3)
--    False
--    (0.22 secs, 91,257,744 bytes)
--    λ> todosPares2 (10^3)
--    False
--    (0.01 secs, 2,535,656 bytes)
--    λ> todosPares3 (10^3)
--    False
--    (0.03 secs, 11,530,528 bytes)
--    λ> todosPares4 (10^3)
--    False
--    (0.24 secs, 91,231,144 bytes)
--    λ> todosPares5 (10^3)
--    False
--    (0.22 secs, 91,231,208 bytes)
