-- Raiz_de_suma_de_tres_cubos.hs
-- Raíz cuadrada de sumas de tres cubos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-Diciembre-2014 (actualizado 12-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Todos los años, en las proximidades del final de año suelen aparecer
-- problemas con propiedades del número del nuevo año. Una sobre el 201
-- es la publicada el martes en la entrada [2015 como raíz de la suma
-- de tres cubos](http://bit.ly/1uYJexg) del blog [Números y algo
-- más](http://bit.ly/1uYJhZV) en la que se pide calcular tres números
-- tales que 2015 sea igual a la raíz cuadrada de la suma de dichos tres
-- números.
--
-- A partir de dicha entrada, se propone el siguiente problema: Definir
-- la sucesión
--    raicesCuadradasDeSumasDe3Cubos :: [Integer]
-- cuyos elementos son los números que se pueden escribir como raíces
-- cuadradas de sumas de tres cubos. Por ejemplo,
--    λ> take 9 raicesCuadradasDeSumasDe3Cubos
--    [6,9,15,27,48,53,59,71,72]
--    λ> raicesCuadradasDeSumasDe3Cubos !! 100
--    672
-- El 6 está en la sucesión porque 1³+2³+3³ = 36 y la raíz cuadrada de
-- 36 es 6 y el 9 está porque 3³+3³+3³ = 81 y la raíz cuadrada de 81 es
-- 9.
--
-- A partir de la sucesión `raicesCuadradasDeSumasDe3Cubos se plantean
-- las siguientes cuestiones:
-- 1. ¿Qué lugar ocupa el 2015 en la sucesión?
-- 2. ¿Cuál será el próximo año que se podrá escribir como la raíz
--    cuadrada de suma de tres cubos?
-- Algunos números tienen varias descomposiones como raíz cuadrada de
-- suma de tres cubos. Por ejemplo, el 71 se puede escribir como la raíz
-- cuadrada de la suma de los cubos de 6, 9 y 16 y también como la de 4,
-- 4, y 17. Lo que plantea otra dos cuestiones
-- 3. ¿Cuáles son las descomposiciones de 2015 como raíz cuadrada de
--    suma de tres cubos?
-- 4. ¿Cuáles son los años hasta el 2015 que se pueden escribir como
--    raíz cuadrada de suma de tres cubos de más formas distintas?
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Raiz_de_suma_de_tres_cubos where

import Data.List (sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

raicesCuadradasDeSumasDe3Cubos1 :: [Integer]
raicesCuadradasDeSumasDe3Cubos1 =
  [n | n <- [1..], not (null (descomposiciones1 n))]

-- (descomposiciones n) es la lista de ternas de números tales que n es la
-- raíz cuadrada de la suma de los cubos de los tres números de la
-- terna. Por ejemplo,
--    descomposiciones  6  ==  [(1,2,3)]
--    descomposiciones  9  ==  [(3,3,3)]
--    descomposiciones 71  ==  [(6,9,16),(4,4,17)]
descomposiciones1 :: Integer -> [(Integer,Integer,Integer)]
descomposiciones1 n =
    [(a,b,c) | a <- [1..n],
               b <- [1..n],
               c <- [1..n],
               a <= b,
               b <= c,
               n^2 == a^3 + b^3 + c^3]

-- 2ª solución
-- ===========

raicesCuadradasDeSumasDe3Cubos2 :: [Integer]
raicesCuadradasDeSumasDe3Cubos2 =
  [n | n <- [1..], not (null (descomposiciones2 n))]

descomposiciones2 :: Integer -> [(Integer,Integer,Integer)]
descomposiciones2 n =
    [(a,b,c) | a <- [1..n],
               b <- [a..n],
               c <- [b..n],
               n^2 == a^3 + b^3 + c^3]

-- 3ª solución
-- ===========

raicesCuadradasDeSumasDe3Cubos3 :: [Integer]
raicesCuadradasDeSumasDe3Cubos3 =
  [n | n <- [1..], not (null (descomposiciones3 n))]

descomposiciones3 :: Integer -> [(Integer,Integer,Integer)]
descomposiciones3 n =
    [(a,b,c) | a <- [1..m],
               b <- [a..m],
               c <- [b..m],
               n^2 == a^3 + b^3 + c^3]
  where m = floor (fromIntegral (n*n)**(1/3))

-- 4ª solución
-- ===========

raicesCuadradasDeSumasDe3Cubos4 :: [Integer]
raicesCuadradasDeSumasDe3Cubos4 =
  [n | n <- [1..], not (null (descomposiciones4 n))]

descomposiciones4 :: Integer -> [(Integer,Integer,Integer)]
descomposiciones4 n =
  [(a,b,c) | c <- [1..m],
             b <- [1..c],
             let d = n^2 - c^3 - b^3,
             d > 0,
             let a = round (fromIntegral d ** (1/3)),
             a <= b,
             a^3 == d]
  where m = floor (fromIntegral (n*n)**(1/3))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Integer] -> Spec
specG raicesCuadradasDeSumasDe3Cubos = do
  it "e1" $
    take 4 raicesCuadradasDeSumasDe3Cubos `shouldBe` [6,9,15,27]

spec :: Spec
spec = do
  describe "def. 1" $ specG raicesCuadradasDeSumasDe3Cubos1
  describe "def. 2" $ specG raicesCuadradasDeSumasDe3Cubos2
  describe "def. 3" $ specG raicesCuadradasDeSumasDe3Cubos3
  describe "def. 4" $ specG raicesCuadradasDeSumasDe3Cubos4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  all (== raicesCuadradasDeSumasDe3Cubos1 !! n)
      [raicesCuadradasDeSumasDe3Cubos2 !! n,
       raicesCuadradasDeSumasDe3Cubos3 !! n,
       raicesCuadradasDeSumasDe3Cubos4 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=9}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> descomposiciones1 253
--    [(1,2,40),(12,25,36)]
--    (10.79 secs, 9,337,643,944 bytes)
--    λ> descomposiciones2 253
--    [(1,2,40),(12,25,36)]
--    (6.29 secs, 7,194,138,952 bytes)
--    λ> descomposiciones3 253
--    [(1,2,40),(12,25,36)]
--    (0.06 secs, 30,965,776 bytes)
--    λ> descomposiciones4 253
--    [(12,25,36),(1,2,40)]
--    (0.01 secs, 2,727,752 bytes)
--
--    λ> raicesCuadradasDeSumasDe3Cubos1 !! 7
--    71
--    (4.09 secs, 3,446,545,944 bytes)
--    λ> raicesCuadradasDeSumasDe3Cubos2 !! 7
--    71
--    (2.29 secs, 2,682,797,856 bytes)
--    λ> raicesCuadradasDeSumasDe3Cubos3 !! 7
--    71
--    (0.08 secs, 55,521,968 bytes)
--    λ> raicesCuadradasDeSumasDe3Cubos4 !! 7
--    71
--    (0.04 secs, 12,254,520 bytes)
--    λ> :r
--    Ok, one module loaded.
--    λ> raicesCuadradasDeSumasDe3Cubos3 !! 40
--    253
--    (1.94 secs, 2,284,895,544 bytes)
--    λ> raicesCuadradasDeSumasDe3Cubos4 !! 40
--    253
--    (0.27 secs, 216,861,896 bytes)

-- Cuestiones
-- ==========

-- El cálculo de la posición de 2015 es
--    λ> length (takeWhile (<=2015) raicesCuadradasDeSumasDe3Cubos4)
--    343

-- El cálculo del próximo año expresable como la raízcuadrada de la suma
-- de tres cubos es
--    λ> head (dropWhile (<=2015) raicesCuadradasDeSumasDe3Cubos4)
--    2022

-- (masDescomponibles xs) es la lista de elementos de xs que se pueden
-- escribir de más formas como raíz cuadrada de suma de tres cubos.
masDescomponibles :: [Integer] -> [Integer]
masDescomponibles xs =
  [y | (_,y) <- takeWhile (\p -> fst p == u) zs]
  where zs = reverse (sort [(length (descomposiciones4 x),x) | x <- xs])
        u  = fst (head zs)

-- El cálculo de los años hasta el 2015 con mayor número de
-- descomposiciones es
--    λ> masDescomponibles (takeWhile (<=2015) raicesCuadradasDeSumasDe3Cubos4)
--    [1728]
