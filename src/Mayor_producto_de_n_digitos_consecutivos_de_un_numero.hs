-- Mayor_producto_de_n_digitos_consecutivos_de_un_numero.hs
-- Mayor producto de n dígitos consecutivos de un número.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-Febrero-2015 (actualizado 8-Febrero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mayorProducto :: Int -> Integer -> Integer
-- tal que (mayorProducto n x) es el mayor producto de n dígitos
-- consecutivos del número x (suponiendo que x tiene al menos n
-- dígitos). Por ejemplo,
--    mayorProducto 2 325                  ==  10
--    mayorProducto 5 11111                ==  1
--    mayorProducto 5 113111               ==  3
--    mayorProducto 5 110111               ==  0
--    mayorProducto 5 10151112             ==  10
--    mayorProducto 5 101511124            ==  10
--    mayorProducto 5 (product [1..1000])  ==  41472
--    mayorProducto 5 (123456^1234689)     ==  59049
-- ---------------------------------------------------------------------

module Mayor_producto_de_n_digitos_consecutivos_de_un_numero where

import Data.List (inits, tails)
import Data.Char (digitToInt)
import Data.List.Split (divvy)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Descomposición con funciones auxiliares
-- ====================================================

mayorProducto1 :: Int -> Integer -> Integer
mayorProducto1 n x =
  maximum [product xs | xs <- segmentos n (cifras x)]

-- (cifras x) es la lista de las cifras del número x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = map (toInteger . digitToInt) (show x)

-- (segmentos n xs) es la lista de los segmentos de longitud n de la
-- lista xs. Por ejemplo,
--    segmentos 2 [3,5,4,6]  ==  [[3,5],[5,4],[4,6]]
segmentos :: Int -> [Integer] -> [[Integer]]
segmentos n xs = take (length xs - n + 1) (map (take n) (tails xs))

-- 2ª solución: Recursión explícita
-- ================================

mayorProducto2 :: Int -> Integer -> Integer
mayorProducto2 n x = maximum (aux ns)
    where ns     = [read [d] | d <- show x]
          aux xs | length xs < n = []
                 | otherwise     = product (take n xs) : aux (tail xs)

-- 3ª solución: Refinamiento usando composición
-- ============================================

mayorProducto3 :: Int -> Integer -> Integer
mayorProducto3 n = maximum
                 . map (product . take n)
                 . filter ((>=n) . length)
                 . tails
                 . cifras

-- 4ª solución: Fuerza bruta con inits y tails
-- ===========================================

mayorProducto4 :: Int -> Integer -> Integer
mayorProducto4 n = maximum
                 . map (product . map (fromIntegral . digitToInt))
                 . filter ((==n) . length)
                 . concatMap inits
                 . tails
                 . show

-- 5ª solución: Optimización basada en el cero
-- ===========================================

mayorProducto5 :: Int -> Integer -> Integer
mayorProducto5 n x = maximum (0 : productos (cifras x))
  where
    productos xs
      | length xs < n = []
      | 0 `elem` take n xs = productos (tail (dropWhile (/= 0) xs))
      | otherwise = product (take n xs) : productos (tail xs)

-- 6ª solución: Uso de la librería split
-- =====================================

mayorProducto6 :: Int -> Integer -> Integer
mayorProducto6 n x =
  maximum (map product (divvy n 1 (cifras x)))

-- 7ª solución: Composición y librería
-- ===================================

mayorProducto7 :: Int -> Integer -> Integer
mayorProducto7 n  =
  maximum . map product . divvy n 1 . cifras

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Integer -> Integer) -> Spec
specG mayorProducto = do
  it "e1" $
    mayorProducto 2 325       `shouldBe` 10
  it "e2" $
    mayorProducto 5 11111     `shouldBe` 1
  it "e3" $
    mayorProducto 5 113111    `shouldBe` 3
  it "e4" $
    mayorProducto 5 110111    `shouldBe` 0
  it "e5" $
    mayorProducto 5 10151112  `shouldBe` 10
  it "e6" $
    mayorProducto 5 101511124 `shouldBe` 10

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorProducto1
  describe "def. 2" $ specG mayorProducto2
  describe "def. 3" $ specG mayorProducto3
  describe "def. 4" $ specG mayorProducto4
  describe "def. 5" $ specG mayorProducto5
  describe "def. 6" $ specG mayorProducto6
  describe "def. 7" $ specG mayorProducto7

-- La verificación es
--    λ> verifica
--    42 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- genEjemplo es un generador de ejemplos (es decir, de pares formados
-- por un Int n y un Integer con al menos n dígitos). Por ejemplo,
--    λ> generate genEjemplo
--    (5,1262536785)
--    λ> generate genEjemplo
--    (10,54879920995089158515)
genEjemplo :: Gen (Int,Integer)
genEjemplo = do
  n <- choose (1,10)
  x <- choose (10^n,10^(2*n))
  return (n,x)

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia =
  forAll genEjemplo $ \(n,x) ->
  all (== mayorProducto1 n x)
      [ mayorProducto2 n x
      , mayorProducto3 n x
      , mayorProducto4 n x
      , mayorProducto5 n x
      , mayorProducto6 n x
      , mayorProducto7 n x
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> mayorProducto1 5 (product [1..500])
--    28224
--    (0.01 secs, 1,759,184 bytes)
--    λ> mayorProducto2 5 (product [1..500])
--    28224
--    (0.03 secs, 5,788,496 bytes)
--    λ> mayorProducto3 5 (product [1..500])
--    28224
--    (0.01 secs, 1,640,984 bytes)
--    λ> mayorProducto4 5 (product [1..500])
--    28224
--    (5.97 secs, 10,932,681,960 bytes)
--    λ> mayorProducto5 5 (product [1..500])
--    28224
--    (0.04 secs, 1,894,184 bytes)
--    λ> mayorProducto6 5 (product [1..500])
--    28224
--    (0.01 secs, 1,731,600 bytes)
--    λ> mayorProducto7 5 (product [1..500])
--    28224
--    (0.01 secs, 1,731,984 bytes)
--
--    λ> mayorProducto1 5 (product [1..10000])
--    59049
--    (0.08 secs, 102,242,784 bytes)
--    λ> mayorProducto2 5 (product [1..10000])
--    59049
--    (2.74 secs, 228,906,528 bytes)
--    λ> mayorProducto3 5 (product [1..10000])
--    59049
--    (3.25 secs, 98,535,616 bytes)
--    λ> mayorProducto5 5 (product [1..10000])
--    59049
--    (2.25 secs, 106,822,520 bytes)
--    λ> mayorProducto6 5 (product [1..10000])
--    59049
--    (0.07 secs, 101,388,248 bytes)
--    λ> mayorProducto7 5 (product [1..10000])
--    59049
--    (0.06 secs, 101,388,632 bytes)
