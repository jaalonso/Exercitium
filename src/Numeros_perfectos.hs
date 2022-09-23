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

module Numeros_perfectos where

import Math.NumberTheory.ArithmeticFunctions (sigma)
import Test.QuickCheck

-- 1ª solución
-- ===========

perfectos1 :: Integer -> [Integer]
perfectos1 n =
  [x | x <- [1..n],
       esPerfecto1 x]

-- (esPerfecto x) se verifica si x es un número perfecto. Por ejemplo,
--    esPerfecto 6  ==  True
--    esPerfecto 8  ==  False
esPerfecto1 :: Integer -> Bool
esPerfecto1 x =
  sumaDivisores1 x - x == x

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
-- se obtiene una nueva definición deperfectos. La usada en la
-- definición anterior es la menos eficiente y la que se usa en la
-- siguiente definición es la más eficiente.

perfectos2 :: Integer -> [Integer]
perfectos2 n =
  [x | x <- [1..n],
       esPerfecto2 x]

esPerfecto2 :: Integer -> Bool
esPerfecto2 x =
  sumaDivisores2 x - x == x

sumaDivisores2 :: Integer -> Integer
sumaDivisores2 = sigma 1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_perfectos :: Positive Integer -> Bool
prop_perfectos (Positive n) =
  perfectos1 n == perfectos2 n

-- La comprobación es
--    λ> quickCheck prop_perfectos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> perfectos1 (4*10^3)
--    [6,28,496]
--    (4.64 secs, 1,606,883,384 bytes)
--    λ> perfectos2 (4*10^3)
--    [6,28,496]
--    (0.02 secs, 9,167,208 bytes)
