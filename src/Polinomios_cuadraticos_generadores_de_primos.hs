-- Polinomios_cuadraticos_generadores_de_primos.hs
-- Polinomios cuadráticos generadores de primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En 1772, Euler publicó que el polinomio n² + n + 41 genera 40 números
-- primos para todos los valores de n entre 0 y 39. Sin embargo, cuando
-- n=40, 40²+40+41 = 40(40+1)+41 es divisible por 41.
--
-- Usando ordenadores, se descubrió que el polinomio n² - 79n + 1601
-- genera 80 números primos para todos los valores de n entre 0 y 79.
--
-- Definir la función
--    generadoresMaximales :: Integer -> (Int,[(Integer,Integer)])
-- tal que (generadoresMaximales n) es el par (m,xs) donde
--    + xs es la lista de pares (x,y) tales que n²+xn+y es uno de los
--      polinomios que genera un número máximo de números primos
--      consecutivos a partir de cero entre todos los polinomios de la
--      forma n²+an+b, con |a| ≤ n y |b| ≤ n y
--    + m es dicho número máximo.
-- Por ejemplo,
--    generadoresMaximales    4  ==  ( 3,[(-2,3),(-1,3),(3,3)])
--    generadoresMaximales    6  ==  ( 5,[(-1,5),(5,5)])
--    generadoresMaximales   41  ==  (41,[(-1,41)])
--    generadoresMaximales   50  ==  (43,[(-5,47)])
--    generadoresMaximales  100  ==  (48,[(-15,97)])
--    generadoresMaximales  200  ==  (53,[(-25,197)])
--    generadoresMaximales 1650  ==  (80,[(-79,1601)])
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Polinomios_cuadraticos_generadores_de_primos where

import Data.List (sort)
import Data.Numbers.Primes (primes, isPrime)
import I1M.PolOperaciones (valor, consPol, polCero)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

generadoresMaximales1 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales1 n =
  (m,[(a,b) | a <- [-n..n], b <- [-n..n], nPrimos a b == m])
  where m = maximum [nPrimos a b | a <- [-n..n], b <- [-n..n]]

-- (nPrimos a b) es el número de primos consecutivos generados por el
-- polinomio n² + an + b a partir de n=0. Por ejemplo,
--    nPrimos (-1) 41     ==  41
--    nPrimos (-79) 1601  ==  80
nPrimos :: Integer -> Integer -> Int
nPrimos a b =
  length (takeWhile isPrime [n*n+a*n+b | n <- [0..]])

-- 2ª solución
-- ===========

-- Notas:
-- 1. Se tiene que b es primo, ya que para n = 0, se tiene que
--    0²+a*0+b = b es primo.
-- 2. Se tiene que 1+a+b es primo, ya que es el valor del polinomio para
--    n = 1.

generadoresMaximales2 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales2 n = (m,map snd zs)
  where xs = [(nPrimos a b,(a,b)) | b <- takeWhile (<=n) primes,
                                    a <- [-n..n],
                                    isPrime(1+a+b)]
        ys = reverse (sort xs)
        m  = fst (head ys)
        zs = takeWhile (\(k,_) -> k == m) ys

-- 3ª solución
-- ===========

generadoresMaximales3 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales3 n = (m,map snd zs)
  where xs = [(nPrimos a b,(a,b)) | b <- takeWhile (<=n) primes,
                                    p <- takeWhile (<=1+2*n) primes,
                                    let a = p-b-1]
        ys = reverse (sort xs)
        m  = fst (head ys)
        zs = takeWhile (\(k,_) -> k == m) ys

-- 4ª solución (con la librería de polinomios)
-- ===========================================

generadoresMaximales4 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales4 n = (m,map snd zs)
  where xs = [(nPrimos2 a b,(a,b)) | b <- takeWhile (<=n) primes,
                                     p <- takeWhile (<=1+2*n) primes,
                                     let a = p-b-1]
        ys = reverse (sort xs)
        m  = fst (head ys)
        zs = takeWhile (\(k,_) -> k == m) ys

-- (nPrimos2 a b) es el número de primos consecutivos generados por el
-- polinomio n² + an + b a partir de n=0. Por ejemplo,
--    nPrimos2 (-1) 41     ==  41
--    nPrimos2 (-79) 1601  ==  80
nPrimos2 :: Integer -> Integer -> Int
nPrimos2 a b =
  length (takeWhile isPrime [valor p n | n <- [0..]])
  where p = consPol 2 1 (consPol 1 a (consPol 0 b polCero))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_generadoresMaximales :: Positive Integer -> Bool
prop_generadoresMaximales (Positive n) =
  all (equivalentes (generadoresMaximales1 n'))
      [generadoresMaximales2 n',
       generadoresMaximales3 n',
       generadoresMaximales4 n']
  where n' = n+1

equivalentes :: (Int,[(Integer,Integer)]) -> (Int,[(Integer,Integer)]) -> Bool
equivalentes (n,xs) (m,ys) =
  n == m && sort xs == sort ys

-- La comprobación es
--    λ> quickCheck prop_generadoresMaximales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> generadoresMaximales1 300
--    (56,[(-31,281)])
--    (2.10 secs, 2,744,382,760 bytes)
--    λ> generadoresMaximales2 300
--    (56,[(-31,281)])
--    (0.17 secs, 382,103,656 bytes)
--    λ> generadoresMaximales3 300
--    (56,[(-31,281)])
--    (0.19 secs, 346,725,872 bytes)
--    λ> generadoresMaximales4 300
--    (56,[(-31,281)])
--    (0.20 secs, 388,509,808 bytes)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + Project Euler (problem 47): [Quadratic primes](http://bit.ly/1b3CSKZ).
-- + Wikipedia. [Formula for primes](http://bit.ly/1b3CZWX).
