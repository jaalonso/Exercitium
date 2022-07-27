-- Numero_de_representaciones_de_n_como_suma_de_dos_cuadrados.hs
-- Número de representaciones de n como suma de dos cuadrados. 
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 04-agosto-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Sea n un número natural cuya factorización prima es
--    n = 2^a * p(1)^b(1) *...* p(n)^b(n) * q(1)^c(1) *...* q(m)^c(m)
-- donde los p(i) son los divisores primos de n congruentes con 3 módulo
-- 4 y los q(j) son los divisores primos de n congruentes con 1 módulo
-- 4 Entonces, el número de forma de descomponer n como suma de dos
-- cuadrados es 0, si algún b(i) es impar y es el techo (es decir, el
-- número entero más próximo por exceso) de
--    ((1+c(1)) *...* (1+c(m))) / 2
-- en caso contrario. Por ejemplo, el número 2^3*(3^9*7^8)*(5^3*13^6) no se
-- puede descomponer como sumas de dos cuadrados (porque el exponente de
-- 3 es impar) y el número 2^3*(3^2*7^8)*(5^3*13^6) tiene 14
-- descomposiciones como suma de dos cuadrados (porque los exponentes de
-- 3 y 7 son pares y el techo de ((1+3)*(1+6))/2 es 14).
--
-- Definir la función
--    nRepresentaciones :: Integer -> Integer
-- tal que (nRepresentaciones n) es el número de formas de representar n
-- como suma de dos cuadrados. Por ejemplo, 
--    nRepresentaciones (2^3*3^9*5^3*7^8*13^6)        ==  0
--    nRepresentaciones (2^3*3^2*5^3*7^8*13^6)        ==  14
--    head [n | n <- [1..], nRepresentaciones n > 8]  ==  71825
--
-- Usando la función representaciones del ejercicio anterior, comprobar
-- con QuickCheck la siguiente propiedad
--    prop_representacion :: Positive Integer -> Bool
--    prop_representacion (Positive n) =
--      nRepresentaciones2 n == genericLength (representaciones n)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numero_de_representaciones_de_n_como_suma_de_dos_cuadrados where

import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

nRepresentaciones1 :: Integer -> Integer
nRepresentaciones1 n =
  ceiling (fromIntegral (product (map aux (factorizacion n))) / 2)
  where aux (p,e) | p == 2         = 1
                  | p `mod` 4 == 3 = if even e then 1 else 0
                  | otherwise      = e+1

-- (factorizacion n) es la factorización prima de n. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n =
  map (\xs -> (head xs, genericLength xs)) (group (primeFactors n))

-- 2ª solución
-- ===========

nRepresentaciones2 :: Integer -> Integer
nRepresentaciones2 n =
  (1 + product (map aux (factorizacion n))) `div`  2
  where aux (p,e) | p == 2         = 1
                  | p `mod` 4 == 3 = if even e then 1 else 0
                  | otherwise      = e+1

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nRepresentaciones :: Positive Integer -> Bool
prop_nRepresentaciones (Positive n) =
  nRepresentaciones1 n == nRepresentaciones2 n
  
-- La comprobación es
--    λ> quickCheck prop_nRepresentaciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> head [n | n <- [1..], nRepresentaciones1 n > 8]
--    71825
--    (1.39 secs, 2,970,063,760 bytes)
--    λ> head [n | n <- [1..], nRepresentaciones2 n > 8]
--    71825
--    (1.71 secs, 2,943,788,424 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_representacion :: Positive Integer -> Bool
prop_representacion (Positive n) =
  nRepresentaciones2 n == genericLength (representaciones n)

representaciones :: Integer -> [(Integer,Integer)]
representaciones n =
  [(x,raiz z) | x <- [0..raiz (n `div` 2)], 
                let z = n - x*x,
                esCuadrado z]

esCuadrado :: Integer -> Bool
esCuadrado x = x == y * y
  where y = raiz x

raiz :: Integer -> Integer
raiz x = floor (sqrt (fromIntegral x))
    
-- La comprobación es
--    λ> quickCheck prop_representacion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + W. Stein, 
--   [Which numbers are the sum of two squares?](http://bit.ly/1Q193xq).
-- + E.W. Weisstein, 
--   [Sum of squares function](http://bit.ly/1Q1c4Oe) en MathWorld.
-- + N.J.A. Sloane,
--   [Sucesión A004018](http://oeis.org/A004018) de OEIS.
-- + [Expressing a number as a sum of two squares](http://bit.ly/20Nr1VY).
-- + [Sum of squares](http://bit.ly/20NrWpp).
-- 
-- La sucesión [nRepresentaciones n | n <- [0..]] es la
-- [A000161](https://oeis.org/A000161) de la OEIS.
