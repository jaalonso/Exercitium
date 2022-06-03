-- Primos_circulares.hs
-- Primos circulares
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un primo circular es un número tal que todas las rotaciones de sus
-- dígitos producen números primos. Por ejemplo, 195 es un primo
-- circular ya que las rotaciones de sus dígitos son 197, 971 y 719 y
-- los tres números son primos.
-- 
-- Definir la constante
--    circulares :: [Integer]
-- cuyo valor es la lista de los números primos circulares. Por ejemplo, 
--    take 16 circulares == [2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197]
--    circulares !! 50   == 933199
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Primos_circulares where

import Data.Numbers.Primes
import Test.QuickCheck

-- 1ª solución
-- ===========
  
circulares1 :: [Integer]
circulares1 = filter esCircular1 primes

-- (esCircular1 n) se verifica si n es un número circular. Por ejemplo, 
--    esCircular1 197  ==  True
--    esCircular1 157  ==  False
esCircular1 :: Integer -> Bool
esCircular1 = all (isPrime . read) . rotaciones1 . show 

-- (rotaciones1 xs) es la lista de las rotaciones obtenidas desplazando
-- el primer elemento xs al final. Por ejemplo,
--    rotaciones1 [2,3,5]  ==  [[2,3,5],[3,5,2],[5,2,3]]
rotaciones1 :: [a] -> [[a]]
rotaciones1 xs = reverse (aux (length xs) [xs])
    where aux 1 yss      = yss
          aux n (ys:yss) = aux (n-1) (rota ys : ys :yss)

-- 2ª solución
-- ===========
  
circulares2 :: [Integer]
circulares2 = filter esCircular2 primes

esCircular2 :: Integer -> Bool
esCircular2 = all (isPrime . read) . rotaciones2 . show 

rotaciones2 :: [a] -> [[a]]
rotaciones2 xs = take (length xs) (iterate rota xs)

-- (rota xs) es la lista añadiendo el primer elemento de xs al
-- final. Por ejemplo, 
--    rota [3,2,5,7]  ==  [2,5,7,3]
rota :: [a] -> [a]
rota (x:xs) = xs ++ [x]

-- 3ª solución
-- ===========

circulares3 :: [Integer]
circulares3 = filter (all isPrime . rotaciones3) primes 

rotaciones3 :: Integer -> [Integer]
rotaciones3 n = [read (take m (drop i (cycle s))) | i <- [1..m]]
    where s = show n
          m = length s

-- 4ª definición
-- =============

-- Nota. La 4ª definición es una mejora observando que para que n sea un
-- número primo circular es necesario que todos los dígitos de n sean
-- impares, salvo para n = 2. 

circulares4 :: [Integer]
circulares4 = 2 : filter esCircular4 primes

-- (esCircular4 n) se verifica si n es un número circular. Por ejemplo, 
--    esCircular4 197  ==  True
--    esCircular4 157  ==  False
esCircular4 :: Integer -> Bool
esCircular4 n = digitosImpares n && 
                all (isPrime . read) (rotaciones2 (show n))

-- (digitosImpares n) se verifica si todos los dígitos de n son
-- impares. Por ejemplo,
--    digitosImpares 7351  ==  True
--    digitosImpares 7341  ==  False
digitosImpares :: Integer -> Bool
digitosImpares = all (`elem` "135679") . show

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_circulares :: Positive Int -> Bool
prop_circulares (Positive n) =
  all (== circulares1 !! n)
      [circulares2 !! n,
       circulares3 !! n,
       circulares4 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=50}) prop_circulares
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> circulares1 !! 46
--    331999
--    (2.08 secs, 7,229,208,200 bytes)
--    λ> circulares2 !! 46
--    331999
--    (1.93 secs, 7,165,043,992 bytes)
--    λ> circulares3 !! 46
--    331999
--    (0.74 secs, 2,469,098,648 bytes)
--    λ> circulares4 !! 46
--    331999
--    (0.28 secs, 917,501,600 bytes)

