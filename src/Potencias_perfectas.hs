-- Potencias_perfectas.hs
-- Potencias perfectas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número natural n es una **potencia perfecta** si existen dos
-- números naturales m > 1 y k > 1 tales que n = m^k. Las primeras
-- potencias perfectas son
--    4 = 2², 8 = 2³, 9 = 3², 16 = 2⁴, 25 = 5², 27 = 3³, 32 = 2⁵,
--    36 = 6², 49 = 7², 64 = 2⁶, ...
--
-- Definir la sucesión
--    potenciasPerfectas :: [Integer]
-- cuyos términos son las potencias perfectas. Por ejemplo,
--    take 10 potenciasPerfectas  ==  [4,8,9,16,25,27,32,36,49,64]
--    potenciasPerfectas !! 3000  ==  7778521
--
-- Definir el procedimiento
--    grafica :: Int -> IO ()
-- tal que (grafica n) es la representación gráfica de las n primeras
-- potencias perfectas. Por ejemplo, para (grafica 30) dibuja
--    Potencias_perfectas.png
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Potencias_perfectas where

import Data.List (group)
import Data.Numbers.Primes (primeFactors)
import Graphics.Gnuplot.Simple (Attribute (Key, PNG), plotList)
import Test.QuickCheck (NonNegative (NonNegative), quickCheck)

-- 1ª solución
-- ===========

potenciasPerfectas1 :: [Integer]
potenciasPerfectas1 = filter esPotenciaPerfecta [4..]

-- (esPotenciaPerfecta x) se verifica si x es una potencia perfecta. Por
-- ejemplo,
--    esPotenciaPerfecta 36  ==  True
--    esPotenciaPerfecta 72  ==  False
esPotenciaPerfecta :: Integer -> Bool
esPotenciaPerfecta = not . null. potenciasPerfectasDe

-- (potenciasPerfectasDe x) es la lista de pares (a,b) tales que
-- x = a^b. Por ejemplo,
--    potenciasPerfectasDe 64  ==  [(2,6),(4,3),(8,2)]
--    potenciasPerfectasDe 72  ==  []
potenciasPerfectasDe :: Integer -> [(Integer,Integer)]
potenciasPerfectasDe n =
  [(m,k) | m <- takeWhile (\x -> x*x <= n) [2..]
         , k <- takeWhile (\x -> m^x <= n) [2..]
         , m^k == n]

-- 2ª solución
-- ===========

potenciasPerfectas2 :: [Integer]
potenciasPerfectas2 = [x | x <- [4..], esPotenciaPerfecta2 x]

-- (esPotenciaPerfecta2 x) se verifica si x es una potencia perfecta. Por
-- ejemplo,
--    esPotenciaPerfecta2 36  ==  True
--    esPotenciaPerfecta2 72  ==  False
esPotenciaPerfecta2 :: Integer -> Bool
esPotenciaPerfecta2 x = mcd (exponentes x) > 1

-- (exponentes x) es la lista de los exponentes de l factorización prima
-- de x. Por ejemplos,
--    exponentes 36  ==  [2,2]
--    exponentes 72  ==  [3,2]
exponentes :: Integer -> [Int]
exponentes x = [length ys | ys <- group (primeFactors x)]

-- (mcd xs) es el máximo común divisor de la lista xs. Por ejemplo,
--    mcd [4,6,10]  ==  2
--    mcd [4,5,10]  ==  1
mcd :: [Int] -> Int
mcd = foldl1 gcd

-- 3ª solución
-- ===========

potenciasPerfectas3 :: [Integer]
potenciasPerfectas3 = mezclaTodas potencias

-- potencias es la lista las listas de potencias de todos los números
-- mayores que 1 con exponentes mayores que 1. Por ejemplo,
--    λ> map (take 3) (take 4 potencias)
--    [[4,8,16],[9,27,81],[16,64,256],[25,125,625]]
potencias :: [[Integer]]
potencias = [[n^k | k <- [2..]] | n <- [2..]]

-- (mezclaTodas xss) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xss. Por ejemplo,
--    take 7 (mezclaTodas potencias)  ==  [4,8,9,16,25,27,32]
mezclaTodas :: Ord a => [[a]] -> [a]
mezclaTodas = foldr1 xmezcla
  where xmezcla (x:xs) ys = x : mezcla xs ys

-- (mezcla xs ys) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xs e ys. Por ejemplo,
--    take 7 (mezcla [2,5..] [4,6..])  ==  [2,4,5,6,8,10,11]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys) | x < y  = x : mezcla xs (y:ys)
                     | x == y = x : mezcla xs ys
                     | x > y  = y : mezcla (x:xs) ys

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_potenciasPerfectas :: NonNegative Int -> Bool
prop_potenciasPerfectas (NonNegative n) =
  all (== potenciasPerfectas1 !! n)
      [potenciasPerfectas2 !! n,
       potenciasPerfectas3 !! n]

-- La comprobación es
--    λ> quickCheck prop_potenciasPerfectas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> potenciasPerfectas1 !! 200
--    28224
--    (10.56 secs, 8,434,647,368 bytes)
--    λ> potenciasPerfectas2 !! 200
--    28224
--    (0.36 secs, 825,040,416 bytes)
--    λ> potenciasPerfectas3 !! 200
--    28224
--    (0.05 secs, 7,474,280 bytes)
--
--    λ> potenciasPerfectas2 !! 500
--    191844
--    (4.16 secs, 9,899,367,112 bytes)
--    λ> potenciasPerfectas3 !! 500
--    191844
--    (0.09 secs, 51,275,464 bytes)

-- En lo que sigue se usa la 3ª solución
potenciasPerfectas :: [Integer]
potenciasPerfectas = potenciasPerfectas3

-- Representación gráfica
-- ======================

grafica :: Int -> IO ()
grafica n =
  plotList [ Key Nothing
           , PNG "Potencias_perfectas.png"
           ]
           (take n potenciasPerfectas)
