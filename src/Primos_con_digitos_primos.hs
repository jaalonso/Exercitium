-- Primos_con_digitos_primos.hs
-- Números primos con todos sus dígitos primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la lista
--    primosConDigitosPrimos :: [Int]
-- cuyos elementos son los números primos con todos sus dígitos
-- primos. Por ejemplo,
--    λ> take 10 primosConDigitosPrimos
--    [2,3,5,7,23,37,53,73,223,227,233,257,277,337,353,373,523,557,577]
-- ---------------------------------------------------------------------

module Primos_con_digitos_primos where

import Data.Numbers.Primes
import Data.Char           -- Para A1
import Data.List (unfoldr) -- Para A5

-- 1ª solución
-- ===========

primosConDigitosPrimos :: [Integer]
primosConDigitosPrimos =
  [n | n <- primes, digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los dígitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Integer -> Bool
digitosPrimos n = all (`elem` "2357") (show n)



-- 2ª definición de digitosPrimos:
digitosPrimos2 :: Integer -> Bool
digitosPrimos2 n = subconjunto (cifras n) [2,3,5,7]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
cifras :: Integer -> [Integer]
cifras n = [read [x] | x <-show n]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo,
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- El cálculo es
--    ghci> length (takeWhile (<2013) primosConDigitosPrimos)
--    84

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- María Ruiz
-- ==========

primosConDigitosPrimosA1 :: [Integer]
primosConDigitosPrimosA1 = [n | n <-[1..], all isPrime (cifrasA1 n)]

cifrasA1 :: Integer -> [Integer]
cifrasA1 n = [read [x] | x <-show n]

-- Otra forma sería construirlos teniendo en cuenta que los únicos
-- dígitos primos son: 2, 3, 5 y 7.

pega :: Int -> Integer -> Integer
pega d n = read ((intToDigit d):show n)

primosConDigitosPrimosA2 :: [Integer]
primosConDigitosPrimosA2 = concat (iterate sig [2,3,5,7])
    where sig xs = concat [map (pega d) xs | d <- [2,3,5,7]]

primosConDigitosPrimosA3 :: [Integer]
primosConDigitosPrimosA3 =
    [2,3,5,7] ++ [10*n+d | n <- primosConDigitosPrimosA3, d <- [2,3,5,7]]

primosConDigitosPrimosA4 :: [Integer]
primosConDigitosPrimosA4 =
    [2,3,5,7] ++
    [read (show n ++ [d]) | n <- primosConDigitosPrimosA4, d <- "2357"]

primosConDigitosPrimosA5 :: [Integer]
primosConDigitosPrimosA5 = concat (unfoldr aux [2,3,5,7])
    where aux xs = Just (xs,[10*n+d | n <- xs, d <- [2,3,5,7]])

-- Eficiencia:
-- primosConDigitosPrimosA1 !! 500          == 25522
-- (0.21 secs, 181795184 bytes)
-- ghci> primosConDigitosPrimosA2 !! 500    == 25522
-- (0.00 secs, 516032 bytes)
-- primosConDigitosPrimosA1 !! (10^4)       == 3235772
-- (14.32 secs, 13225392688 bytes)
-- ghci> primosConDigitosPrimosA2 !! (10^4) == 3235772
-- (0.00 secs, 1552256 bytes)
-- primosConDigitosPrimosA2 !! (10^6)       == 5375727572
-- (0.19 secs, 100960620 bytes)
