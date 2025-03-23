-- Valor_de_un_polinomio.hs
-- Valores de polinomios representados con vectores.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 8-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los polinomios se pueden representar mediante vectores usando la
-- librería Data.Array. En primer lugar, se define el tipo de los
-- polinomios (con coeficientes de tipo a) mediante
--    type Polinomio a = Array Int a
-- Como ejemplos, definimos el polinomio
--    ej_pol1 :: Array Int Int
--    ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
-- que representa a 2x - 5x^2 + 7x^4 + 6 y el polinomio
--    ej_pol2 :: Array Int Double
--    ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]
-- que representa a 2x - 5.2x^2 + 7x^4 + 6.5
--
-- Definir la función
--    valor :: Num a => Polinomio a -> a -> a
-- tal que (valor p b) es el valor del polinomio p en el punto b. Por
-- ejemplo,
--    valor ej_pol1 0  ==  6
--    valor ej_pol1 1  ==  10
--    valor ej_pol1 2  ==  102
--    valor ej_pol2 0  ==  6.5
--    valor ej_pol2 1  ==  10.3
--    valor ej_pol2 3  ==  532.7
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module A2014.M05.Valor_de_un_polinomio where

import Data.List (foldl')
import Data.Array (Array, (!), array, assocs, bounds, elems, listArray)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Polinomio a = Array Int a

ej_pol1 :: Array Int Int
ej_pol1 = array (0,4) [(0,6),(1,2),(2,-5),(3,0),(4,7)]

ej_pol2 :: Array Int Double
ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]

-- 1ª solución
-- ===========

valor1 :: Num a => Polinomio a -> a -> a
valor1 p b = sum [(p!i)*b^i | i <- [0..n]]
  where (_,n) = bounds p

-- 2ª solución
-- ===========

valor2 :: Num a => Polinomio a -> a -> a
valor2 p b = sum [(p!i)*b^i | i <- [0..length p - 1]]

-- 3ª solución
-- ===========

valor3 :: Num a => Polinomio a -> a -> a
valor3 p b = sum [v*b^i | (i,v) <- assocs p]

-- 4ª solución
-- ===========

valor4 :: Num a => Polinomio a -> a -> a
valor4 = valorLista4 . elems

valorLista4 :: Num a => [a] -> a -> a
valorLista4 xs b =
  sum [(xs !! i) * b^i | i <- [0..length xs - 1]]

-- 5ª solución
-- ===========

valor5 :: Num a => Polinomio a -> a -> a
valor5 = valorLista5 . elems

valorLista5 :: Num a => [a] -> a -> a
valorLista5 []     _ = 0
valorLista5 (x:xs) b = x + b * valorLista5 xs b

-- 6ª solución
-- ===========

valor6 :: Num a => Polinomio a -> a -> a
valor6 = valorLista6 . elems

valorLista6 :: Num a => [a] -> a -> a
valorLista6 xs b = aux xs
  where aux []     = 0
        aux (y:ys) = y + b * aux ys

-- 7ª solución
-- ===========

valor7 :: Num a => Polinomio a -> a -> a
valor7 = valorLista7 . elems

valorLista7 :: Num a => [a] -> a -> a
valorLista7 xs b = foldr (\y r -> y + b * r) 0 xs

-- 8ª solución
-- ===========

valor8 :: Num a => Polinomio a -> a -> a
valor8 = valorLista8 . elems

valorLista8 :: Num a => [a] -> a -> a
valorLista8 xs b = aux 0 (reverse xs)
  where aux r []     = r
        aux r (y:ys) = aux (y + r * b) ys

-- 9ª solución
-- ===========

valor9 :: Num a => Polinomio a -> a -> a
valor9 = valorLista9 . elems

valorLista9 :: Num a => [a] -> a -> a
valorLista9 xs b = aux 0 (reverse xs)
  where aux = foldl (\ r y -> y + r * b)

-- 10ª solución
-- ============

valor10 :: Num a => Polinomio a -> a -> a
valor10 p b =
  foldl (\ r y -> y + r * b) 0 (reverse (elems p))

-- 11ª solución
-- ============

valor11 :: Num a => Polinomio a -> a -> a
valor11 p b =
  foldl' (\ r y -> y + r * b) 0 (reverse (elems p))

-- 12ª solución
-- ============

valor12 :: Num a => Polinomio a -> a -> a
valor12 p b =
  sum (zipWith (*) (elems p) (iterate (* b) 1))

-- 13ª solución
-- ============

valor13 :: Num a => Polinomio a -> a -> a
valor13 p b =
  foldl' (+) 0 (zipWith (*) (elems p) (iterate (* b) 1))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: (Polinomio Int -> Int -> Int) -> Spec
specG1 valor = do
  it "e1" $
    valor ej_pol1 0  `shouldBe`  6
  it "e2" $
    valor ej_pol1 1  `shouldBe`  10
  it "e3" $
    valor ej_pol1 2  `shouldBe`  102

specG2 :: (Polinomio Double -> Double -> Double) -> Spec
specG2 valor = do
  it "e4" $
    valor ej_pol2 0  `shouldBe`  6.5
  it "e5" $
    valor ej_pol2 1  `shouldBe`  10.3
  it "e6" $
    abs (valor ej_pol2 3 - 532.7) < 0.1 `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG1 valor1
  describe "def. 2" $ specG1 valor2
  describe "def. 3" $ specG1 valor3
  describe "def. 4" $ specG1 valor4
  describe "def. 5" $ specG1 valor5
  describe "def. 1" $ specG2 valor1
  describe "def. 2" $ specG2 valor2
  describe "def. 3" $ specG2 valor3
  describe "def. 4" $ specG2 valor4
  describe "def. 5" $ specG2 valor5

-- La verificación es
--    λ> verifica
--    30 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_valor :: [Integer] -> Integer -> Bool
prop_valor xs b =
  all (== valor1 p b)
      [f p b | f <- [valor2,
                     valor3,
                     valor4,
                     valor5,
                     valor6,
                     valor7,
                     valor8,
                     valor9,
                     valor10,
                     valor11,
                     valor12,
                     valor13]]
  where p = listArray (0, length xs - 1) xs

-- La comprobación es
--    λ> quickCheck prop_valor
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (valor1 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (7.62 secs, 2,953,933,864 bytes)
--    λ> length (show (valor2 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (8.26 secs, 2,953,933,264 bytes)
--    λ> length (show (valor3 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (7.49 secs, 2,954,733,184 bytes)
--    λ> length (show (valor4 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (84.80 secs, 2,956,333,712 bytes)
--    λ> length (show (valor5 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.34 secs, 1,307,347,416 bytes)
--    λ> length (show (valor6 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.26 secs, 1,308,114,752 bytes)
--    λ> length (show (valor7 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.21 secs, 1,296,843,456 bytes)
--    λ> length (show (valor8 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.28 secs, 1,309,591,744 bytes)
--    λ> length (show (valor9 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.27 secs, 1,299,191,672 bytes)
--    λ> length (show (valor10 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (1.30 secs, 1,299,191,432 bytes)
--    λ> length (show (valor11 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (0.23 secs, 1,287,654,752 bytes)
--    λ> length (show (valor12 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (0.75 secs, 1,309,506,968 bytes)
--    λ> length (show (valor13 (listArray (0,10^5) (repeat 1)) 2))
--    30104
--    (0.22 secs, 1,298,867,128 bytes)
