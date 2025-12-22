-- Numeros_con_factorizacion_capicua.hs
-- 2015 y los números con factorización capicúa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-Enero-2015 (actualizado 22-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número tiene factorización capicúa* si puede escribir como un
-- producto de números primos tal que la concatenación de sus dígitos
-- forma un número capicúa. Por ejemplo, el 2015 tiene factorización
-- capicúa ya que 2015 = 13*5*31, los factores son primos y su
-- concatenación es 13531 que es capicúa.
--
-- Definir la sucesión
--    conFactorizacionesCapicuas :: [Int]
-- formada por los números que tienen factorización capicúa. Por
-- ejemplo,
--    λ> take 20 conFactorizacionesCapicuas
--    [1,2,3,4,5,7,8,9,11,12,16,18,20,25,27,28,32,36,39,44]
--    λ> conFactorizacionesCapicuas !! 3000
--    91019
--
-- Usando conFactorizacionesCapicuas escribir expresiones cuyos valores
-- sean las respuestas a las siguientes preguntas y calcularlas
--    1. ¿Qué lugar ocupa el 2015 en la sucesión?
--    2. ¿Cuál fue el anterior año con factorización capicúa?
--    3. ¿Cuál será el siguiente año con factorización capicúa?
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_con_factorizacion_capicua where

import Data.List (delete, group, nub, permutations, sort)
import Data.Numbers.Primes (primeFactors)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ============

conFactorizacionesCapicuas1 :: [Int]
conFactorizacionesCapicuas1 =
  [n | n <- [1..], not (null (factorizacionesCapicua n))]

-- (factorizacionesCapicua n) es la lista de las factorizaciones
-- capicúas de n. Por ejemplo,
--    factorizacionesCapicua 2015  ==  [[13,5,31],[31,5,13]]
factorizacionesCapicua :: Int -> [[Int]]
factorizacionesCapicua n =
  [xs | xs <- permutations (factorizacion n),
        esCapicuaConcatenacion xs]

-- (factorizacion n) es la lista de todos los factores primos de n; es
-- decir, es una lista de números primos cuyo producto es n. Por ejemplo,
--    factorizacion 300  ==  [2,2,3,5,5]
factorizacion :: Int -> [Int]
factorizacion n | n == 1    = []
                | otherwise = x : factorizacion (div n x)
  where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
--    menorFactor 16  ==  2
--    menorFactor 17  == 17
menorFactor :: Int -> Int
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- (esCapicuaConcatenacion xs) se verifica si la concatenación de los
-- números de xs es capicúa. Por ejemplo,
--    esCapicuaConcatenacion [13,5,31]   ==  True
--    esCapicuaConcatenacion [135,31]    ==  True
--    esCapicuaConcatenacion [135,21]    ==  False
esCapicuaConcatenacion :: [Int] -> Bool
esCapicuaConcatenacion xs =
  esCapicua (concatMap show xs)

-- (esCapicua s) se verifica si la cadena s es capicúa.
esCapicua :: String -> Bool
esCapicua s = s == reverse s

-- 2ª solución
-- ===========

conFactorizacionesCapicuas2 :: [Int]
conFactorizacionesCapicuas2 =
  [n | n <- [1..], not (null (factorizacionesCapicua2 n))]

factorizacionesCapicua2 :: Int -> [[Int]]
factorizacionesCapicua2 n =
  [xs | xs <- permutaciones (factorizacion2 n),
        esCapicuaConcatenacion xs]

factorizacion2 :: Int -> [Int]
factorizacion2 1 = []
factorizacion2 n = factoriza n 2
  where
    factoriza m d
      | d * d > m    = [m]
      | m `mod` d == 0 = d : factoriza (m `div` d) d
      | otherwise      = factoriza m (d + 1)

-- (permutaciones xs) es la lista sin repeticiones de las permutaciones
-- de xs. Por ejemplo,
--    λ> permutaciones [2,2,3] == [[2,2,3],[2,3,2],[3,2,2]]
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x:ps | x <- nub xs, ps <- permutaciones (delete x xs)]

-- 3ª solución
-- ===========

conFactorizacionesCapicuas3 :: [Int]
conFactorizacionesCapicuas3 =
  [n | n <- [1..], not (null (factorizacionesCapicua3 n))]

-- (factorizacionesCapicua3 n) es la lista de las factorizaciones
-- capicúas de n. Por ejemplo,
--    factorizacionesCapicua3 2015  ==  [[13,5,31],[31,5,13]]
factorizacionesCapicua3 :: Int -> [[Int]]
factorizacionesCapicua3 n =
  [xs | xs <- permutaciones (primeFactors n),
        esCapicuaConcatenacion xs]

-- 4ª solución
-- ===========

conFactorizacionesCapicuas4 :: [Int]
conFactorizacionesCapicuas4 =
  [n | n <- [1..], conFactorizacionCapicua n]

-- (conFactorizacionCapicua n) se verifica si n tiene factorización
-- capicúa. Por ejemplo,
--    factorizacionesCapicua2 2015  ==  [[13,5,31],[31,5,13]]
conFactorizacionCapicua :: Int -> Bool
conFactorizacionCapicua n =
  any listaCapicua (permutaciones (primeFactors n))

-- (listaCapicua xs) se verifica si la cadena obtenida concatenando los
-- dígitos de xs es capicúa. Por ejemplo,
--    listaCapicua [13,5,31] == True
--    listaCapicua [13,5,21] == False
listaCapicua :: Show a => [a] -> Bool
listaCapicua xs = ys == reverse ys
  where ys = concatMap show xs

-- 5ª solución
-- ===========

conFactorizacionesCapicuas5 :: [Int]
conFactorizacionesCapicuas5 = filter tieneFactorizacionCapicua  [1..]

-- (tieneFactorizacionCapicua n) se verifica si n tiene alguna
-- factorización capicúa. Por ejemplo,
--    tieneFactorizacionCapicua 2015 == True
--    tieneFactorizacionCapicua 2016 == False
tieneFactorizacionCapicua :: Int -> Bool
tieneFactorizacionCapicua n =
  any ((\ s -> s == reverse s) . concat)
      (permutaciones $ map show $ factorizacion5 n)

factorizacion5 :: Int -> [Int]
factorizacion5 n
  | n < 2     = []
  | primo n   = [n]
  | otherwise = divs ++ factorizacion5 (n `div` product divs)
  where primo       = null . divisores
        divisores m = [x | x <- [2..m `div` 2], m `mod` x == 0]
        divs        = filter primo (divisores n)

-- 6ª solución
-- ===========

conFactorizacionesCapicuas6 :: [Int]
conFactorizacionesCapicuas6 = filter tieneFactorizacionCapicua6 [1..]

tieneFactorizacionCapicua6 :: Int -> Bool
tieneFactorizacionCapicua6 1 = True
tieneFactorizacionCapicua6 n =
  puedeSerCapicua fs &&
  any (esCapicua . concat) (permutaciones (map show fs))
  where fs = factorizacion6 n

factorizacion6 :: Int -> [Int]
factorizacion6 1 = []
factorizacion6 n = aux n (2 : [3,5..])
  where
    aux m (p:ps) | p * p > m     = [m]
                 | m `mod` p == 0 = p : aux (m `div` p) (p:ps)
                 | otherwise      = aux m ps
    aux _ _ = error "Caso imposible"

-- (puedeSerCapicua fs) se verifica si a lo sumo un dígito de fs aparece
-- un número impar de veces. Por ejemplo,
--    puedeSerCapicua [5,13,31] == True
--    puedeSerCapicua [5,13,21] == False
puedeSerCapicua :: [Int] -> Bool
puedeSerCapicua fs =
  length (filter odd (frecuencias (digitos fs))) <= 1

-- (digitos xs) es la cadena de los dígitos de xs. Por ejemplo,
--    λ> digitos [3,11,3,13]
--    "311313"
digitos :: [Int] -> String
digitos = concatMap show

-- (frecuencias cs) es la lista del número de apariciones de cada uno de
-- los elementos de cs. Por ejemplo.
--    λ> frecuencias "3533573"
--    [4,2,1]
frecuencias :: String -> [Int]
frecuencias cs = map length $ group $ sort cs

-- 7ª solución
-- ===========

conFactorizacionesCapicuas7 :: [Int]
conFactorizacionesCapicuas7 = filter tieneFactorizacionCapicua7 [1..]

tieneFactorizacionCapicua7 :: Int -> Bool
tieneFactorizacionCapicua7 1 = True
tieneFactorizacionCapicua7 n =
  puedeSerCapicua fs &&
  any (esCapicua . concat) (permutaciones (map show fs))
  where fs = primeFactors n

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Int] -> Spec
specG conFactorizacionesCapicuas = do
  it "e1" $
    take 20 conFactorizacionesCapicuas
    `shouldBe` [1,2,3,4,5,7,8,9,11,12,16,18,20,25,27,28,32,36,39,44]

spec :: Spec
spec = do
  describe "def. 1" $ specG conFactorizacionesCapicuas1
  describe "def. 2" $ specG conFactorizacionesCapicuas2
  describe "def. 3" $ specG conFactorizacionesCapicuas3
  describe "def. 4" $ specG conFactorizacionesCapicuas4
  describe "def. 5" $ specG conFactorizacionesCapicuas5
  describe "def. 6" $ specG conFactorizacionesCapicuas6
  describe "def. 7" $ specG conFactorizacionesCapicuas7

-- La verificación es
--    λ> verifica
--    7 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
    all (== conFactorizacionesCapicuas1 !! n)
        [conFactorizacionesCapicuas2 !! n,
         conFactorizacionesCapicuas3 !! n,
         conFactorizacionesCapicuas4 !! n,
         conFactorizacionesCapicuas5 !! n,
         conFactorizacionesCapicuas6 !! n,
         conFactorizacionesCapicuas7 !! n]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> conFactorizacionesCapicuas1 !! 300
--    2450
--    (5.23 secs, 10,907,294,432 bytes)
--    λ> conFactorizacionesCapicuas2 !! 300
--    2450
--    (0.14 secs, 98,081,344 bytes)
--
--    λ> conFactorizacionesCapicuas2 !! 400
--    3861
--    (0.26 secs, 172,766,536 bytes)
--    λ> conFactorizacionesCapicuas3 !! 400
--    3861
--    (0.15 secs, 186,914,864 bytes)
--    λ> conFactorizacionesCapicuas4 !! 400
--    3861
--    (0.20 secs, 189,234,456 bytes)
--    λ> conFactorizacionesCapicuas5 !! 400
--    3861
--    (2.17 secs, 1,349,678,296 bytes)
--    λ> conFactorizacionesCapicuas6 !! 400
--    3861
--    (0.12 secs, 54,761,808 bytes)
--    λ> conFactorizacionesCapicuas7 !! 400
--    3861
--    (0.09 secs, 85,802,672 bytes)
--
--    λ> conFactorizacionesCapicuas2 !! 1000
--    15885
--    (1.49 secs, 1,331,680,880 bytes)
--    λ> conFactorizacionesCapicuas3 !! 1000
--    15885
--    (1.11 secs, 1,397,445,856 bytes)
--    λ> conFactorizacionesCapicuas4 !! 1000
--    15885
--    (1.19 secs, 1,414,891,080 bytes)
--    λ> conFactorizacionesCapicuas6 !! 1000
--    15885
--    (0.42 secs, 312,621,920 bytes)
--    λ> conFactorizacionesCapicuas7 !! 1000
--    15885
--    (0.27 secs, 515,702,608 bytes)
--
--    λ> conFactorizacionesCapicuas6 !! 2000
--    47916
--    (1.71 secs, 1,286,865,688 bytes)
--    λ> conFactorizacionesCapicuas7 !! 2000
--    47916
--    (1.02 secs, 2,123,255,960 bytes)

-- Cuestiones
-- ==========

-- El cálculo de la 1ª respuesta es
--    λ> length (takeWhile (<= 2015) conFactorizacionesCapicuas)
--    265

-- El cálculo de la 2ª respuesta es
--    λ> last (takeWhile (<2015) conFactorizacionesCapicuas)
--    2001

-- El cálculo de la 3ª respuesta es
--    λ> head (dropWhile (<=2015) conFactorizacionesCapicuas)
--    2023
