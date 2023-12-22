-- El_teorema_de_Navidad_de_Fermat.hs
-- El teorema de Navidad de Fermat.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-diciembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El 25 de diciembre de 1640, en una carta a Mersenne, Fermat demostró
-- la conjetura de Girard: todo primo de la forma 4n+1 puede expresarse
-- de manera única como suma de dos cuadrados. Por eso es conocido como
-- el Teorema de Navidad de Fermat
--
-- Definir las funciones
--    representaciones :: Integer -> [(Integer,Integer)]
--    primosImparesConRepresentacionUnica :: [Integer]
--    primos4nM1 :: [Integer]
-- tales que
-- + (representaciones n) es la lista de pares de números naturales
--   (x,y) tales que n = x^2 + y^2 con x <= y. Por ejemplo.
--      representaciones  20           ==  [(2,4)]
--      representaciones  25           ==  [(0,5),(3,4)]
--      representaciones 325           ==  [(1,18),(6,17),(10,15)]
--      representaciones 100000147984  ==  [(0,316228)]
--      length (representaciones (10^10))    ==  6
--      length (representaciones (4*10^12))  ==  7
-- + primosImparesConRepresentacionUnica es la lista de los números
--   primos impares que se pueden escribir exactamente de una manera
--   como suma de cuadrados de pares de números naturales (x,y) con
--   x <= y. Por ejemplo,
--      λ> take 20 primosImparesConRepresentacionUnica
--      [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
-- + primos4nM1 es la lista de los números primos que se pueden escribir
--   como uno más un múltiplo de 4 (es decir, que son congruentes con 1
--   módulo 4). Por ejemplo,
--      λ> take 20 primos4nM1
--      [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
--
-- El [teorema de Navidad de Fermat](http://bit.ly/2Roso1o) afirma que
-- un número primo impar p se puede escribir exactamente de una manera
-- como suma de dos cuadrados de números naturales p = x² + y^2 (con
-- x <= y) si, y sólo si, p se puede escribir como uno más un múltiplo
-- de 4 (es decir, que es congruente con 1 módulo 4).

-- Comprobar con QuickCheck el teorema de Navidad de Fermat; es decir,
-- que para todo número n, los n-ésimos elementos de
-- primosImparesConRepresentacionUnica y de primos4nM1 son iguales.
-- ---------------------------------------------------------------------

module El_teorema_de_Navidad_de_Fermat where

import Data.Numbers.Primes (primes)
import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª definición de representaciones
-- =================================

representaciones :: Integer -> [(Integer,Integer)]
representaciones n =
  [(x,y) | x <- [0..n], y <- [x..n], n == x*x + y*y]

-- 2ª definición de representaciones
-- =================================

representaciones2 :: Integer -> [(Integer,Integer)]
representaciones2 n =
  [(x,raiz z) | x <- [0..raiz (n `div` 2)]
              , let z = n - x*x
              , esCuadrado z]

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Integer -> Bool
esCuadrado x = x == y * y
  where y = raiz x

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

-- 3ª definición de representaciones
-- =================================

representaciones3 :: Integer -> [(Integer,Integer)]
representaciones3 n =
  [(x,raiz3 z) | x <- [0..raiz3 (n `div` 2)]
               , let z = n - x*x
               , esCuadrado3 z]

-- (esCuadrado3 x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado3 25  ==  True
--    esCuadrado3 26  ==  False
esCuadrado3 :: Integer -> Bool
esCuadrado3 x = x == y * y
  where y = raiz3 x

-- (raiz3 x) es la raíz cuadrada entera de x. Por ejemplo,
--    raiz3 25  ==  5
--    raiz3 24  ==  4
--    raiz3 26  ==  5
raiz3 :: Integer -> Integer
raiz3 x = floor (sqrt (fromIntegral x))

-- 4ª definición de representaciones
-- =================================

representaciones4 :: Integer -> [(Integer, Integer)]
representaciones4 n = aux 0 (floor (sqrt (fromIntegral n)))
  where aux x y
          | x > y     = []
          | otherwise = case compare (x*x + y*y) n of
                          LT -> aux (x + 1) y
                          EQ -> (x, y) : aux (x + 1) (y - 1)
                          GT -> aux x (y - 1)

-- Equivalencia de las definiciones de representaciones
-- ====================================================

-- La propiedad es
prop_representaciones_equiv :: Positive Integer -> Bool
prop_representaciones_equiv (Positive n) =
  representaciones  n == representaciones2 n &&
  representaciones2 n == representaciones3 n &&
  representaciones3 n == representaciones4 n

-- La comprobación es
--    λ> quickCheck prop_representaciones_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de las definiciones de representaciones
-- =================================================================

--    λ> representaciones 3025
--    [(0,55),(33,44)]
--    (2.86 secs, 1,393,133,528 bytes)
--    λ> representaciones2 3025
--    [(0,55),(33,44)]
--    (0.00 secs, 867,944 bytes)
--    λ> representaciones3 3025
--    [(0,55),(33,44)]
--    (0.00 secs, 173,512 bytes)
--    λ> representaciones4 3025
--    [(0,55),(33,44)]
--    (0.00 secs, 423,424 bytes)
--
--    λ> length (representaciones2 (10^10))
--    6
--    (3.38 secs, 2,188,903,544 bytes)
--    λ> length (representaciones3 (10^10))
--    6
--    (0.10 secs, 62,349,048 bytes)
--    λ> length (representaciones4 (10^10))
--    6
--    (0.11 secs, 48,052,360 bytes)
--
--    λ> length (representaciones3 (4*10^12))
--    7
--    (1.85 secs, 1,222,007,176 bytes)
--    λ> length (representaciones4 (4*10^12))
--    7
--    (1.79 secs, 953,497,480 bytes)

-- Definición de primosImparesConRepresentacionUnica
-- =================================================

primosImparesConRepresentacionUnica :: [Integer]
primosImparesConRepresentacionUnica =
  [x | x <- tail primes
     , length (representaciones4 x) == 1]

-- Definición de primos4nM1
-- ========================

primos4nM1 :: [Integer]
primos4nM1 = [x | x <- primes
                , x `mod` 4 == 1]

-- Teorema de Navidad de Fermat
-- ============================

-- La propiedad es
prop_teoremaDeNavidadDeFermat :: Positive Int -> Bool
prop_teoremaDeNavidadDeFermat (Positive n) =
  primosImparesConRepresentacionUnica !! n == primos4nM1 !! n

-- La comprobación es
--    λ> quickCheck prop_teoremaDeNavidadDeFermat
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    representaciones  20 `shouldBe`  [(2,4)]
  it "e2" $
    representaciones  25 `shouldBe`  [(0,5),(3,4)]
  it "e3" $
    representaciones 325 `shouldBe`  [(1,18),(6,17),(10,15)]
  it "e4" $
    take 20 primosImparesConRepresentacionUnica `shouldBe`
     [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]
  it "e5" $
    take 20 primos4nM1 `shouldBe`
     [5,13,17,29,37,41,53,61,73,89,97,101,109,113,137,149,157,173,181,193]

-- La verificación es
--    λ> verifica
--
--    5 examples, 0 failures
