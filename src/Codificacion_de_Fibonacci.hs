-- Codificacion_de_Fibonacci.hs
-- Codificación de Fibonacci.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La codificación de Fibonacci http://bit.ly/1Lllqjv de un número n es
-- una cadena d = d(0)d(1)...d(k-1)d(k) de ceros y unos tal que
--    n = d(0)*F(2) + d(1)*F(3) +...+ d(k-1)*F(k+1) 
--    d(k-1) = d(k) = 1
-- donde F(i) es el i-ésimo término de la sucesión de Fibonacci
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
-- Por ejemplo, la codificación de Fibonacci de 4 es "1011" ya que los
-- dos últimos elementos son iguales a 1 y
--    1*F(2) + 0*F(3) + 1*F(4) = 1*1 + 0*2 + 1*3 = 4
-- La codificación de Fibonacci de los primeros números se muestra en la
-- siguiente tabla
--     1  = 1     = F(2)           ≡       11
--     2  = 2     = F(3)           ≡      011
--     3  = 3     = F(4)           ≡     0011
--     4  = 1+3   = F(2)+F(4)      ≡     1011
--     5  = 5     = F(5)           ≡    00011
--     6  = 1+5   = F(2)+F(5)      ≡    10011
--     7  = 2+5   = F(3)+F(5)      ≡    01011
--     8  = 8     = F(6)           ≡   000011
--     9  = 1+8   = F(2)+F(6)      ≡   100011
--    10  = 2+8   = F(3)+F(6)      ≡   010011
--    11  = 3+8   = F(4)+F(6)      ≡   001011
--    12  = 1+3+8 = F(2)+F(4)+F(6) ≡   101011
--    13  = 13    = F(7)           ≡  0000011
--    14  = 1+13  = F(2)+F(7)      ≡  1000011
--
-- Definir la función
--    codigoFib :: Integer -> String
-- tal que (codigoFib n) es la codificación de Fibonacci del número
-- n. Por ejemplo,
--    λ> codigoFib 65
--    "0100100011"
--    λ> [codigoFib n | n <- [1..7]]
--    ["11","011","0011","1011","00011","10011","01011"]
--
-- Comprobar con QuickCheck las siguientes propiedades:
-- + Todo entero positivo se puede descomponer en suma de números de
--   la sucesión de Fibonacci.
-- + Las codificaciones de Fibonacci tienen como mínimo 2 elementos.
-- + En las codificaciones de Fibonacci, la cadena "11" sólo
--   aparece una vez y la única vez que aparece es al final.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Codificacion_de_Fibonacci where

import Data.List (isInfixOf)
import Data.Array (Array, accumArray, elems)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

codigoFib1 :: Integer -> String
codigoFib1 = concatMap show . codificaFibLista

-- (codificaFibLista n) es la lista correspondiente a la codificación de
-- Fibonacci del número n. Por ejemplo,
--    λ> codificaFibLista 65
--    [0,1,0,0,1,0,0,0,1,1]
--    λ> [codificaFibLista n | n <- [1..7]]
--    [[1,1],[0,1,1],[0,0,1,1],[1,0,1,1],[0,0,0,1,1],[1,0,0,1,1],[0,1,0,1,1]]
codificaFibLista :: Integer -> [Integer]
codificaFibLista n = map f [2..head xs] ++ [1]
  where xs = map fst (descomposicion n)
        f i | i `elem` xs = 1
            | otherwise = 0

-- (descomposicion n) es la lista de pares (i,f) tales que f es el
-- i-ésimo número de Fibonacci y las segundas componentes es una
-- sucesión decreciente de números de Fibonacci cuya suma es n. Por
-- ejemplo, 
--    descomposicion 65  ==  [(10,55),(6,8),(3,2)]
--    descomposicion 66  ==  [(10,55),(6,8),(4,3)]
descomposicion :: Integer -> [(Integer, Integer)]
descomposicion 0 = []
descomposicion 1 = [(2,1)]
descomposicion n = (i,x) : descomposicion (n-x)
  where (i,x) = fibAnterior n

-- (fibAnterior n) es el mayor número de Fibonacci menor o igual que
-- n. Por ejemplo,
--    fibAnterior 33  ==  (8,21)
--    fibAnterior 34  ==  (9,34)
fibAnterior :: Integer -> (Integer, Integer)
fibAnterior n = last (takeWhile p fibsConIndice)
  where p (_,x) = x <= n

-- fibsConIndice es la sucesión de los números de Fibonacci junto con
-- sus índices. Por ejemplo,
--    λ> take 10 fibsConIndice
--    [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13),(8,21),(9,34)]
fibsConIndice :: [(Integer, Integer)]
fibsConIndice = zip [0..] fibs

-- fibs es la sucesión de Fibonacci. Por ejemplo, 
--    take 10 fibs  ==  [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--- 2ª solución
-- ============

codigoFib2 :: Integer -> String
codigoFib2 = concatMap show . elems . codificaFibVec

-- (codificaFibVec n) es el vector correspondiente a la codificación de
-- Fibonacci del número n. Por ejemplo,
--    λ> codificaFibVec 65
--    array (0,9) [(0,0),(1,1),(2,0),(3,0),(4,1),(5,0),(6,0),(7,0),(8,1),(9,1)]
--    λ> [elems (codificaFibVec n) | n <- [1..7]]
--    [[1,1],[0,1,1],[0,0,1,1],[1,0,1,1],[0,0,0,1,1],[1,0,0,1,1],[0,1,0,1,1]]
codificaFibVec :: Integer -> Array Integer Integer
codificaFibVec n = accumArray (+) 0 (0,a+1) ((a+1,1):is) 
  where is = [(i-2,1) | (i,_) <- descomposicion n]
        a  = fst (head is)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_codigoFib :: Positive Integer -> Bool 
prop_codigoFib (Positive n) =
  codigoFib1 n == codigoFib2 n
  
-- La comprobación es
--    λ> quickCheck prop_codigoFib
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> head [n | n <- [1..], length (codigoFib1 n) > 25]
--    121393
--    (4.30 secs, 3,031,108,104 bytes)
--    λ> head [n | n <- [1..], length (codigoFib2 n) > 25]
--    121393
--    (3.46 secs, 2,505,869,616 bytes)

-- Propiedades
-- ===========

-- Usaremos la 2ª definición
codigoFib :: Integer -> String
codigoFib = codigoFib2

-- Prop.: La función descomposicion es correcta:
prop_descomposicion_correcta :: Positive Integer -> Bool
prop_descomposicion_correcta (Positive n) =
  n == sum (map snd (descomposicion n))

-- La comprobación es
--    λ> quickCheck prop_descomposicion_correcta
--    +++ OK, passed 100 tests.

-- Prop.: Todo entero positivo se puede descomponer en suma de números de
-- la sucesión de Fibonacci.
prop_descomposicion :: Positive Integer -> Bool
prop_descomposicion (Positive n) =
  not (null (descomposicion n))

-- La comprobación es
--    λ> quickCheck prop_descomposicion
--    +++ OK, passed 100 tests.

-- Prop.: Las codificaciones de Fibonacci tienen como mínimo 2 elementos.
prop_length_codigoFib :: Positive Integer -> Bool
prop_length_codigoFib (Positive n) =
  length (codigoFib n) >= 2

-- La comprobación es
--    λ> quickCheck prop_length_codigoFib
--    +++ OK, passed 100 tests.

-- Prop.: En las codificaciones de Fibonacci, la cadena "11" sólo
-- aparece una vez y la única vez que aparece es al final.
prop3_cadena_11_en_codigoFib :: Positive Integer -> Bool
prop3_cadena_11_en_codigoFib (Positive n) = 
  take 2 xs == "11" && not ("11" `isInfixOf` drop 2 xs)
  where xs = reverse (codigoFib n)

-- La comprobación es
--    λ> quickCheck prop3_cadena_11_en_codigoFib
--    +++ OK, passed 100 tests.
