-- Representaciones_de_un_numero_como_suma_de_dos_cuadrados.hs
-- Representaciones de un número como suma de dos cuadrados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-enero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    representaciones :: Integer -> [(Integer,Integer)]
-- tal que (representaciones n) es la lista de pares de números
-- naturales (x,y) tales que n = x^2 + y^2. Por ejemplo.
--    representaciones  20              ==  [(2,4)]
--    representaciones  25              ==  [(0,5),(3,4)]
--    representaciones 325              ==  [(1,18),(6,17),(10,15)]
--    length (representaciones (10^14)) == 8
--
-- Comprobar con QuickCheck que un número natural n se puede representar
-- como suma de dos cuadrados si, y sólo si, en la factorización prima
-- de n todos los exponentes de sus factores primos congruentes con 3
-- módulo 4 son pares.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Representaciones_de_un_numero_como_suma_de_dos_cuadrados where

import Data.List (genericLength, group)
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

representaciones1 :: Integer -> [(Integer,Integer)]
representaciones1 n =
  [(x,y) | x <- [0..n], y <- [x..n], n == x*x + y*y]

-- 2ª solución
-- ===========

representaciones2 :: Integer -> [(Integer,Integer)]
representaciones2 n =
  [(x,raiz z) | x <- [0..raiz (n `div` 2)],
                let z = n - x*x,
                esCuadrado z]

-- (raiz x) es la raíz cuadrada entera de x. Por ejemplo,
--    raiz 25  ==  5
--    raiz 24  ==  4
--    raiz 26  ==  5
raiz :: Integer -> Integer
raiz 0 = 0
raiz 1 = 1
raiz x = aux (0,x)
    where aux (a,b) | d == x    = c
                    | c == a    = a
                    | d < x     = aux (c,b)
                    | otherwise = aux (a,c)
              where c = (a+b) `div` 2
                    d = c^2

-- Nota: La siguiente definición de raíz cuadrada falla para números
-- grandes.
--    raiz' :: Integer -> Integer
--    raiz' = floor . sqrt . fromIntegral
-- Por ejemplo,
--    λ> raiz' (10^50)
--    9999999999999998758486016
--    λ> raiz (10^50)
--    10000000000000000000000000

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Integer -> Bool
esCuadrado x = x == y * y
  where y = raiz x

-- 3ª solución
-- ===========

representaciones3 :: Integer -> [(Integer, Integer)]
representaciones3 n = aux 0 (raiz n)
  where aux x y
          | x > y     = []
          | otherwise = case compare (x*x + y*y) n of
                          LT -> aux (x + 1) y
                          EQ -> (x, y) : aux (x + 1) (y - 1)
                          GT -> aux x (y - 1)

-- Verificación                                                     --
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> [(Integer, Integer)]) -> Spec
specG representaciones = do
  it "e1" $
    representaciones  20 `shouldBe` [(2,4)]
  it "e2" $
    representaciones  25 `shouldBe` [(0,5),(3,4)]
  it "e3" $
    representaciones 325 `shouldBe` [(1,18),(6,17),(10,15)]


spec :: Spec
spec = do
  describe "def. 1" $ specG representaciones1
  describe "def. 2" $ specG representaciones2
  describe "def. 3" $ specG representaciones3

-- La verificación es
--    λ> verifica
--
--    9 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_representaciones :: Positive Integer -> Bool
prop_representaciones (Positive n) =
  all (== representaciones1 n)
      [representaciones2 n,
       representaciones3 n]

-- La comprobación es
--    λ> quickCheck prop_representaciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> representaciones1 4000
--    [(20,60),(36,52)]
--    (4.95 secs, 2,434,929,624 bytes)
--    λ> representaciones2 4000
--    [(20,60),(36,52)]
--    (0.00 secs, 599,800 bytes)
--    λ> representaciones3 4000
--    [(20,60),(36,52)]
--    (0.01 secs, 591,184 bytes)
--
--    λ> length (representaciones2 (10^14))
--    8
--    (6.64 secs, 5,600,837,088 bytes)
--    λ> length (representaciones3 (10^14))
--    8
--    (9.37 secs, 4,720,548,264 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_representacion :: Positive Integer -> Bool
prop_representacion (Positive n) =
  not (null (representaciones2 n)) ==
  all (\(p,e) -> p `mod` 4 /= 3 || even e) (factorizacion n)

-- (factorizacion n) es la factorización prima de n. Por ejemplo,
--    factorizacion 600  ==  [(2,3),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n =
  map (\xs -> (head xs, genericLength xs)) (group (primeFactors n))

-- La comprobación es
--    λ> quickCheck prop_representacion
--    +++ OK, passed 100 tests.
