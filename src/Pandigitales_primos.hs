-- Pandigitales_primos.hs
-- Pandigitales primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número con n dígitos es pandigital si contiene todos los dígitos
-- del 1 a n exactamente una vez. Por ejemplo, 2143 es un pandigital con
-- 4 dígitos y, además, es primo.
--
-- Definir la constante
--    pandigitalesPrimos :: [Int]
-- tal que sus elementos son los números pandigitales primos, ordenados
-- de mayor a menor. Por ejemplo,
--    take 3 pandigitalesPrimos       ==  [7652413,7642513,7641253]
--    2143 `elem` pandigitalesPrimos  ==  True
--    length pandigitalesPrimos       ==  538
-- ---------------------------------------------------------------------

module Pandigitales_primos where

import Data.List (permutations, sort)
import Data.Char (intToDigit)
import Data.Numbers.Primes (isPrime)

-- 1ª solución
-- ===========

pandigitalesPrimos1 :: [Int]
pandigitalesPrimos1 =
  concatMap nPandigitalesPrimos1 [9,8..1]

-- (nPandigitalesPrimos n) es la lista de los números pandigitales con n
-- dígitos, ordenada de mayor a menor. Por ejemplo,
--    nPandigitalesPrimos 4  ==  [4231,2341,2143,1423]
--    nPandigitalesPrimos 5  ==  []
nPandigitalesPrimos1 :: Int -> [Int]
nPandigitalesPrimos1 n = filter isPrime (pandigitales n)

-- (pandigitales n) es la lista de los números pandigitales de n dígitos
-- ordenada de mayor a menor. Por ejemplo,
--    pandigitales 3  ==  [321,312,231,213,132,123]
pandigitales :: Int -> [Int]
pandigitales n = 
    reverse $ sort $ map digitosAentero (permutations [1..n])

-- (digitosAentero ns) es el número cuyos dígitos son ns. Por ejemplo,
--    digitosAentero [3,2,5]  ==  325
digitosAentero :: [Int] -> Int
digitosAentero = read . map intToDigit

-- 2ª solución
-- ===========

pandigitalesPrimos2 :: [Int]
pandigitalesPrimos2 =
  concatMap nPandigitalesPrimos2 [9,8..1]

-- Nota. La definición de nPandigitalesPrimos1 se puede simplificar, ya
-- que la suma de los números de 1 a n es divisible por 3, entonces los
-- números  pandigitales con n dígitos también lo son y, por tanto, no
-- son primos. 
nPandigitalesPrimos2 :: Int -> [Int]
nPandigitalesPrimos2 n 
  | sum [1..n] `mod` 3 == 0 = []
  | otherwise               = filter isPrime (pandigitales n)

-- 2ª solución
-- ===========

pandigitalesPrimos3 :: [Int]
pandigitalesPrimos3 =
  concatMap nPandigitalesPrimos3 [9,8..1]

-- La definición de nPandigitales se puede simplificar, ya que
--    λ> [n | n <- [1..9], sum [1..n] `mod` 3 /= 0]
--    [1,4,7]
nPandigitalesPrimos3 :: Int -> [Int]
nPandigitalesPrimos3 n 
  | n `elem` [4,7] = filter isPrime (pandigitales n)
  | otherwise      = []

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_pandigitalesPrimos :: Bool
prop_pandigitalesPrimos =
  all (== pandigitalesPrimos1)
      [pandigitalesPrimos2,
       pandigitalesPrimos3]

-- La comprobación es
--    λ> prop_pandigitalesPrimos
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (pandigitalesPrimos1)
--    538
--    (1.44 secs, 5,249,850,032 bytes)
--    λ> length (pandigitalesPrimos2)
--    538
--    (0.14 secs, 619,249,632 bytes)
--    λ> length (pandigitalesPrimos3)
--    538
--    (0.14 secs, 619,237,464 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- + Ejercicio 41 del proyecto Euler https://projecteuler.net/problem=41
