-- Mayor_sucesion_3n_mas_1.hs
-- Mayor sucesión del problema 3n+1.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-Junio-2014 (actualizado 1-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La sucesión 3n+1 generada por un número entero positivo x es la
-- sucesión generada por el siguiente algoritmo: Se empieza con el
-- número x. Si x es par, se divide entre 2. Si x es impar, se
-- multiplica por 3 y se le suma 1. El  proceso se repite con el número
-- obtenido hasta que se alcanza el valor 1. Por ejemplo, la sucesión de
-- números generadas cuando se empieza en 22 es
--    22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1
-- Se ha conjeturado (aunque no demostrado) que este algoritmo siempre
-- alcanza el 1 empezando en cualquier entero positivo.
--
-- Definir la función
--    mayorLongitud :: Integer -> Integer -> Integer
-- tal que (mayorLongitud i j) es el máximo de las longitudes de las
-- sucesiones 3n+1 para todos los números comprendidos entre i y j,
-- ambos inclusives. Por ejemplo,
--    mayorLongitud   1   10  ==  20
--    mayorLongitud 100  200  ==  125
--    mayorLongitud 201  210  ==  89
--    mayorLongitud 900 1000  ==  174
-- ---------------------------------------------------------------------

module Mayor_sucesion_3n_mas_1 where

import Data.List (genericLength)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mayorLongitud1 :: Integer -> Integer -> Integer
mayorLongitud1 i j = maximum [genericLength (sucesion k) | k <- [i..j]]

-- (sucesion n) es la sucesión 3n+1 generada por n. Por ejemplo,
--    sucesion 22  ==  [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
sucesion :: Integer -> [Integer]
sucesion 1 = [1]
sucesion n | even n    = n : sucesion (n `div` 2)
           | otherwise = n : sucesion (3*n+1)

-- 2ª solución
-- ===========

mayorLongitud2 :: Integer -> Integer -> Integer
mayorLongitud2 i j = maximum [longitud k | k <- [i..j]]

-- (longitud n) es la longitud de la sucesión 3n+1 generada por n. Por
-- ejemplo,
--    longitud 22  ==  16
longitud :: Integer -> Integer
longitud 1 = 1
longitud n | even n    = 1 + longitud (n `div` 2)
           | otherwise = 1 + longitud (3*n+1)

-- 3ª solución (con iterate)
-- =========================

mayorLongitud3 :: Integer -> Integer -> Integer
mayorLongitud3 i j = maximum [genericLength (sucesion2 k) | k <- [i..j]]

-- (sucesion2 n) es la sucesión 3n+1 generada por n. Por ejemplo,
--    sucesion2 22  ==  [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
sucesion2 :: Integer -> [Integer]
sucesion2 n = takeWhile (/=1) (iterate f n) ++ [1]
    where f x | even x    = x `div` 2
              | otherwise = 3*x+1

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer -> Integer) -> Spec
specG mayorLongitud = do
  it "e1" $
    mayorLongitud   1   10  `shouldBe`  20
  it "e2" $
    mayorLongitud 100  200  `shouldBe`  125
  it "e3" $
    mayorLongitud 201  210  `shouldBe`  89
  it "e4" $
    mayorLongitud 900 1000  `shouldBe`  174

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorLongitud1
  describe "def. 2" $ specG mayorLongitud2
  describe "def. 3" $ specG mayorLongitud3

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_mayorLongitud :: Positive Integer -> Positive Integer -> Bool
prop_mayorLongitud (Positive i) (Positive j) =
  all (== mayorLongitud1 i (i+j))
      [mayorLongitud2 i (i+j),
       mayorLongitud3 i (i+j)]

-- La comprobación es
--    λ> quickCheck prop_mayorLongitud
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mayorLongitud1 1 40000
--    324
--    (2.61 secs, 1,714,320,680 bytes)
--    λ> mayorLongitud2 1 40000
--    324
--    (2.64 secs, 1,457,194,704 bytes)
--    λ> mayorLongitud3 1 40000
--    324
--    (3.77 secs, 2,660,488,832 bytes)
