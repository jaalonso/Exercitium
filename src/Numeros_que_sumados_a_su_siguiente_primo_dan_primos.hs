-- Numeros_que_sumados_a_su_siguiente_primo_dan_primos.hs
-- Números que sumados a su siguiente primo dan primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-Diciembre-2014 (actualizado 7-Diciembre-2025)
-- ---------------------------------------------------------------------

-- La "Enciclopedia electrónica de sucesiones de enteros" (OEIS por sus
-- siglas en inglés, de "On-Line Encyclopedia of Integer Sequences") es
-- una base de datos que registra sucesiones de números enteros. Está
-- disponible libremente en Internet, en la dirección
-- http://oeis.org
--
-- La semana pasada Antonio Roldán añadió una nueva sucesión a la OEIS,
-- la [A249624](https://oeis.org/A249624) que sirve de base para el
-- problema de hoy.
--
-- Definir la sucesión
--     a249624 :: [Integer]
-- tal que sus elementos son los números x tales que la suma de x y el
-- primo que le sigue es un número primo. Por ejemplo,
--    λ> take 20 a249624
--    [0,1,2,6,8,14,18,20,24,30,34,36,38,48,50,54,64,68,78,80]
--
-- El número 8 está en la sucesión porque su siguiente primo es 11 y
-- 8+11=19 es primo. El 12 no está en la sucesión porque su siguiente
-- primo es 13 y 12+13=25 no es primo.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Numeros_que_sumados_a_su_siguiente_primo_dan_primos where

import Data.Numbers.Primes (primes, isPrime)
import Data.List (genericReplicate)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

a249624a :: [Integer]
a249624a = 0 : 1 : [x | x <- [2,4..], primo (x + siguientePrimo x)]

primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

siguientePrimo :: Integer -> Integer
siguientePrimo x = head [y | y <- [x+1..], primo y]

-- 2ª solución
-- ===========

a249624b :: [Integer]
a249624b = 0 : 1 : 2: aux [2,4..] primos where
  aux (x:xs) (y:ys)
    | y < x                = aux (x:xs) ys
    | (x+y) `pertenece` ys = x : aux xs (y:ys)
    | otherwise            = aux xs (y:ys)
  pertenece x ys = x == head (dropWhile (<x) ys)

primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- 3ª solución
-- ===========

a249624c :: [Integer]
a249624c = 0 : 1 : [x | x <- [2,4..], isPrime (x + siguientePrimo3 x)]

siguientePrimo3 :: Integer -> Integer
siguientePrimo3 x = head [y | y <- [x+1..], isPrime y]

-- 4ª solución
-- ===========

a249624d :: [Integer]
a249624d = 0 : 1 : 2: aux [2,4..] primes where
  aux (x:xs) (y:ys)
    | y < x                = aux (x:xs) ys
    | (x+y) `pertenece` ys = x : aux xs (y:ys)
    | otherwise            = aux xs (y:ys)
  pertenece x ys = x == head (dropWhile (<x) ys)

-- 5ª solución
-- ===========

a249624e :: [Integer]
a249624e = [a | q <- primes,
                let p = siguientePrimo3 (q `div` 2),
                let a = q-p,
                siguientePrimo3 a == p]

-- 6ª solución
-- ===========

a249624f :: [Integer]
a249624f = [x | (x,y) <- zip [0..] ps, isPrime (x+y)]
    where ps = 2:2:concat (zipWith f primes (tail primes))
          f p q = genericReplicate (q-p) q

-- 7ª solución
-- ===========

a249624g :: [Integer]
a249624g = 0 : 1 : aux primes (tail primes) primes
  where aux (x:xs) (y:ys) zs
          | null rs   = aux xs ys zs2
          | otherwise = [r-y | r <- rs] ++ aux xs ys zs2
          where a   = x+y
                b   = 2*y-1
                zs1 = takeWhile (<=b) zs
                rs  = [r | r <- [a..b], r `elem` zs1]
                zs2 = dropWhile (<=b) zs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG a249624 = do
  it "e1" $
    take 20 a249624
    `shouldBe` [0,1,2,6,8,14,18,20,24,30,34,36,38,48,50,54,64,68,78,80]

spec :: Spec
spec = do
  describe "def. 1" $ specG a249624a
  describe "def. 2" $ specG a249624b
  describe "def. 3" $ specG a249624c
  describe "def. 4" $ specG a249624d
  describe "def. 5" $ specG a249624e
  describe "def. 6" $ specG a249624f
  describe "def. 7" $ specG a249624g

-- La verificación es
--    λ> verifica
--    7 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Positive Int -> Bool
prop_equivalencia (Positive n) =
  all (== a249624a !! n)
      [a249624b !! n,
       a249624c !! n,
       a249624d !! n,
       a249624e !! n,
       a249624f !! n,
       a249624g !! n]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> a249624a !! 700
--    5670
--    (4.49 secs, 2,394,882,432 bytes)
--    λ> a249624b !! 700
--    5670
--    (2.89 secs, 1,467,707,792 bytes)
--    λ> a249624c !! 700
--    5670
--    (0.11 secs, 204,935,928 bytes)
--    λ> a249624d !! 700
--    5670
--    (0.11 secs, 6,343,968 bytes)
--    λ> a249624e !! 700
--    5670
--    (0.10 secs, 147,394,176 bytes)
--    λ> a249624f !! 700
--    5670
--    (0.06 secs, 58,374,608 bytes)
--    λ> a249624g !! 700
--    5670
--    (0.04 secs, 8,979,720 bytes)
