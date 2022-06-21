-- Numeros_autodescriptivos.hs
-- Números autodescriptivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número n es autodescriptivo cuando para cada posición k de n
-- (empezando a contar las posiciones a partir de 0), el dígito en la
-- posición k es igual al número de veces que ocurre k en n. Por
-- ejemplo, 1210 es autodescriptivo porque tiene 1 dígito igual a "0", 2
-- dígitos iguales a "1", 1 dígito igual a "2" y ningún dígito igual a
-- "3". 
--
-- Definir la función
--    autodescriptivo :: Integer -> Bool
-- tal que (autodescriptivo n) se verifica si n es autodescriptivo. Por
-- ejemplo, 
--    λ> autodescriptivo 1210
--    True
--    λ> [x | x <- [1..100000], autodescriptivo x]
--    [1210,2020,21200]
--    λ> autodescriptivo 9210000001000
--    True
-- ---------------------------------------------------------------------

module Numeros_autodescriptivos where

import Data.Char (digitToInt)
import Data.List (genericLength)
import Test.QuickCheck

-- 1ª solución
-- ===========

autodescriptivo1 :: Integer -> Bool
autodescriptivo1 n = autodescriptiva (digitos n)

-- (digitos n) es la lista de los dígitos de n. Por ejemplo.
--    digitos 325 == [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [d] | d <- show n]

-- (autodescriptiva ns) se verifica si la lista de dígitos ns es
-- autodescriptiva; es decir, si para cada posición k de ns
-- (empezando a contar las posiciones a partir de 0), el dígito en la
-- posición k es igual al número de veces que ocurre k en ns. Por
-- ejemplo, 
--    autodescriptiva [1,2,1,0] == True
--    autodescriptiva [1,2,1,1] == False
autodescriptiva :: [Integer] -> Bool
autodescriptiva ns = 
  and [x == ocurrencias k ns | (k,x) <- zip [0..] ns]

-- (ocurrencias x ys) es el número de veces que ocurre x en ys. Por
-- ejemplo, 
--    ocurrencias 1 [1,2,1,0,1] == 3
ocurrencias :: Integer -> [Integer] -> Integer
ocurrencias x ys = genericLength (filter (==x) ys)

-- 2ª solución
-- ===========

autodescriptivo2 :: Integer -> Bool
autodescriptivo2 n =
  and (zipWith (==) (map digitToInt xs)
                    [length (filter (==c) xs) |  c <- ['0'..'9']])
  where xs = show n

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_autodescriptivo :: Positive Integer -> Bool
prop_autodescriptivo (Positive n) =
  autodescriptivo1 n == autodescriptivo2 n

-- La comprobación es
--    λ> quickCheck prop_autodescriptivo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> [x | x <- [1..3*10^5], autodescriptivo1 x]
--    [1210,2020,21200]
--    (2.59 secs, 6,560,244,696 bytes)
--    λ> [x | x <- [1..3*10^5], autodescriptivo2 x]
--    [1210,2020,21200]
--    (0.67 secs, 425,262,848 bytes)
