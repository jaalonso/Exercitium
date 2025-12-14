-- Numeros_cuya_suma_de_digitos_es_su_numero_de_divisores.hs
-- 2015 y los números con suma de dígitos igual al número de sus divisores
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-Diciembre-2014 (actualizado 13-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una propiedad del 2015 es que la suma de sus dígitos coincide con el
-- número de sus divisores; en efecto, la suma de sus dígitos es
-- 2+0+1+5=8 y tiene 8 divisores (1, 5, 13, 31, 65, 155, 403 y 2015).
--
-- Definir la sucesión
--    especiales :: [Int]
-- formada por los números n tales que la suma de los dígitos de n
-- coincide con el número de divisores de n. Por ejemplo,
--    λ> take 15 especiales
--    [1,2,11,22,36,84,101,152,156,170,202,208,225,228,288]
--    λ> length (takeWhile (<300000) especiales)
--    4305
--
-- Usar la sucesión para responder las siguientes cuestiones
-- + ¿Cuántos años hasta el 2015 inclusive han cumplido la propiedad?
-- + ¿Cuál fue el anterior al 2015 que cumplió la propiedad?
-- + ¿Cuál será el siguiente al 2015 que cumplirá la propiedad?
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_cuya_suma_de_digitos_es_su_numero_de_divisores where

import Data.List (group, inits, nub, sort, subsequences)
import Data.Numbers.Primes (primeFactors)
import Math.NumberTheory.ArithmeticFunctions (divisors, tau)
import Data.Set (size)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

especiales1 :: [Int]
especiales1 = [n | n <- [1..], esEspecial1 n]

esEspecial1 :: Int -> Bool
esEspecial1 n = sum (digitos n) == length (divisores n)

digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

divisores :: Int -> [Int]
divisores n = n : [x | x <- [1..n `div` 2], n `mod` x == 0]

-- 2ª solución
-- ===========

especiales2 :: [Int]
especiales2 = filter esEspecial2 [1..]

esEspecial2 :: Int -> Bool
esEspecial2 n = sum (digitos n) == length (divisores2 n)

divisores2 :: Int -> [Int]
divisores2 n = n : filter ((== 0) . mod n) [1..n `div` 2]

-- 3ª solución
-- ===========

especiales3 :: [Int]
especiales3 = filter esEspecial3 [1..]

esEspecial3 :: Int -> Bool
esEspecial3 n = sum (digitos n) == length (divisores3 n)

divisores3 :: Int -> [Int]
divisores3 =
  nub . sort . map product . subsequences . primeFactors

-- 4ª solución
-- ===========

especiales4 :: [Int]
especiales4 = filter esEspecial4 [1..]

esEspecial4 :: Int -> Bool
esEspecial4 n = sum (digitos n) == length (divisores4 n)

divisores4 :: Int -> [Int]
divisores4 =
  sort
  . map (product . concat)
  . productoCartesiano
  . map inits
  . group
  . primeFactors

-- (productoCartesiano xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    λ> productoCartesiano [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
productoCartesiano :: [[a]] -> [[a]]
productoCartesiano []       = [[]]
productoCartesiano (xs:xss) =
  [x:ys | x <- xs, ys <- productoCartesiano xss]

-- 5ª solución
-- ===========

especiales5 :: [Int]
especiales5 = filter esEspecial5 [1..]

esEspecial5 :: Int -> Bool
esEspecial5 n = sum (digitos n) == length (divisores5 n)

divisores5 :: Int -> [Int]
divisores5 = sort
           . map (product . concat)
           . sequence
           . map inits
           . group
           . primeFactors

-- 6ª solución
-- ===========

especiales6 :: [Int]
especiales6 = filter esEspecial6 [1..]

esEspecial6 :: Int -> Bool
esEspecial6 n = sum (digitos n) == length (divisores6 n)

divisores6 :: Int -> [Int]
divisores6 = sort
           . map (product . concat)
           . mapM inits
           . group
           . primeFactors

-- 7ª solución
-- ===========

especiales7 :: [Int]
especiales7 = filter esEspecial7 [1..]

esEspecial7 :: Int -> Bool
esEspecial7 n = sum (digitos n) == numeroDivisores n

numeroDivisores :: Int -> Int
numeroDivisores =
  product . map ((+1) . length) . group . primeFactors

-- 8ª solución
-- ===========

especiales8 :: [Int]
especiales8 = filter esEspecial8 [1..]

esEspecial8 :: Int -> Bool
esEspecial8 n = sum (digitos n) == size (divisors n)

-- 9ª solución
-- ===========

especiales9 :: [Int]
especiales9 = filter esEspecial9 [1..]

esEspecial9 :: Int -> Bool
esEspecial9 n = sum (digitos n) == tau n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Int] -> Spec
specG especiales = do
  it "e1" $
    take 12 especiales `shouldBe` [1,2,11,22,36,84,101,152,156,170,202,208]

spec :: Spec
spec = do
  describe "def. 1" $ specG especiales1
  describe "def. 2" $ specG especiales2
  describe "def. 3" $ specG especiales3
  describe "def. 4" $ specG especiales4
  describe "def. 5" $ specG especiales5
  describe "def. 6" $ specG especiales6
  describe "def. 7" $ specG especiales7
  describe "def. 8" $ specG especiales8
  describe "def. 9" $ specG especiales9

-- La verificación es
--    λ> verifica
--    9 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  all (== especiales1 !! n)
      [especiales2 !! n,
       especiales3 !! n,
       especiales4 !! n,
       especiales5 !! n,
       especiales6 !! n,
       especiales7 !! n,
       especiales8 !! n,
       especiales9 !! n]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (takeWhile (<5000) especiales1)
--    126
--    (2.28 secs, 1,339,082,248 bytes)
--    λ> length (takeWhile (<5000) especiales2)
--    126
--    (0.79 secs, 834,335,552 bytes)
--    λ> length (takeWhile (<5000) especiales3)
--    126
--    (0.07 secs, 80,114,952 bytes)
--    λ> length (takeWhile (<5000) especiales4)
--    126
--    (0.09 secs, 77,867,744 bytes)
--    λ> length (takeWhile (<5000) especiales5)
--    126
--    (0.06 secs, 86,993,376 bytes)
--    λ> length (takeWhile (<5000) especiales6)
--    126
--    (0.07 secs, 86,666,984 bytes)
--    λ> length (takeWhile (<5000) especiales7)
--    126
--    (0.10 secs, 156,959,160 bytes)
--
--    λ> length (takeWhile (<10000) especiales2)
--    211
--    (2.98 secs, 3,158,460,512 bytes)
--    λ> length (takeWhile (<10000) especiales3)
--    211
--    (0.23 secs, 509,390,408 bytes)
--    λ> length (takeWhile (<10000) especiales4)
--    211
--    (0.33 secs, 495,972,688 bytes)
--    λ> length (takeWhile (<10000) especiales5)
--    211
--    (0.19 secs, 444,606,504 bytes)
--    λ> length (takeWhile (<10000) especiales6)
--    211
--    (0.19 secs, 442,925,088 bytes)
--    λ> length (takeWhile (<10000) especiales7)
--    211
--    (0.17 secs, 354,799,456 bytes)
--    λ> length (takeWhile (<10000) especiales8)
--    211
--    (0.11 secs, 189,371,176 bytes)
--    λ> length (takeWhile (<10000) especiales9)
--    211
--    (0.11 secs, 166,356,248 bytes)
--
--    λ> length (takeWhile (<70000) especiales3)
--    1211
--    (1.93 secs, 5,685,776,168 bytes)
--    λ> length (takeWhile (<70000) especiales4)
--    1211
--    (2.51 secs, 5,130,847,728 bytes)
--    λ> length (takeWhile (<70000) especiales5)
--    1211
--    (1.59 secs, 4,651,436,176 bytes)
--    λ> length (takeWhile (<70000) especiales6)
--    1211
--    (1.61 secs, 4,638,880,024 bytes)
--    λ> length (takeWhile (<70000) especiales7)
--    1211
--    (1.23 secs, 3,861,065,832 bytes)
--    λ> length (takeWhile (<70000) especiales8)
--    1211
--    (0.60 secs, 1,610,471,400 bytes)
--    λ> length (takeWhile (<70000) especiales9)
--    1211
--    (0.47 secs, 1,414,203,176 bytes)
--
--    λ> length (takeWhile (<300000) especiales8)
--    4305
--    (2.67 secs, 7,907,800,408 bytes)
--    λ> :r
--    Ok, one module loaded.
--    λ> length (takeWhile (<300000) especiales9)
--    4305
--    (2.12 secs, 6,952,070,256 bytes)

-- Cuestiones
-- ==========

-- El cálculo de número de años hasta el 2015 inclusive que han cumplido
-- la propiedad es
--    λ> length (takeWhile (<=2015) especiales1)
--    59

-- El cálculo del anterior al 2015 que cumplió la propiedad es
--    λ> last (takeWhile (<2015) especiales1)
--    2006

-- El cálculo del siguiente al 2015 que cumplirá la propiedad es
--    λ> head (dropWhile (<=2015) especiales1)
--    2101
