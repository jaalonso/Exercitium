-- Matriz_zigzagueante.hs
-- Matriz zigzagueante.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La matriz zizagueante de orden n es la matriz cuadrada con n filas y
-- n columnas y cuyos elementos son los n² primeros números naturales
-- colocados de manera creciente a lo largo de las diagonales
-- secundarias. Por ejemplo, La matriz zigzagueante de orden 5 es
--     0  1  5  6 14
--     2  4  7 13 15
--     3  8 12 16 21
--     9 11 17 20 22
--    10 18 19 23 24
-- La colocación de los elementos se puede ver gráficamente en
-- http://bit.ly/1DeO2FI
--
-- Definir la función
--    zigZag :: oInt -> Matrix Int
-- tal que (zigZag n) es la matriz zigzagueante de orden n. Por ejemplo,
--    λ> zigZag1 5
--    ┌                ┐
--    │  0  1  5  6 14 │
--    │  2  4  7 13 15 │
--    │  3  8 12 16 21 │
--    │  9 11 17 20 22 │
--    │ 10 18 19 23 24 │
--    └                ┘
--    λ> zigZag1 4
--    ┌             ┐
--    │  0  1  5  6 │
--    │  2  4  7 12 │
--    │  3  8 11 13 │
--    │  9 10 14 15 │
--    └             ┘
--    λ> maximum (zigZag 1500)
--    2249999
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Matriz_zigzagueante where

import Data.List (sort, sortBy)
import Data.Matrix (Matrix, fromList)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

zigZag1 :: Int -> Matrix Int
zigZag1 n = fromList n n (elementosZigZag n)

-- (elementosZigZag n) es la lista de los elementos de la matriz
-- zizagueante de orden n. Por ejemplo.
--    λ> elementosZigZag 5
--    [0,1,5,6,14,2,4,7,13,15,3,8,12,16,21,9,11,17,20,22,10,18,19,23,24]
elementosZigZag :: Int -> [Int]
elementosZigZag n =
  map snd (sort (zip (ordenZigZag n) [0..]))

-- (ordenZigZag n) es la lista de puntos del cuadrado nxn recorridos en
-- zig-zag por las diagonales secundarias. Por ejemplo,
--    λ> ordenZigZag 4
--    [(1,1), (1,2),(2,1), (3,1),(2,2),(1,3), (1,4),(2,3),(3,2),(4,1),
--     (4,2),(3,3),(2,4), (3,4),(4,3), (4,4)]
ordenZigZag :: Int -> [(Int,Int)]
ordenZigZag n = concat [aux n m | m <- [2..2*n]]
    where aux k m | odd m     = [(x,m-x) | x <- [max 1 (m-k)..min k (m-1)]]
                  | otherwise = [(m-x,x) | x <- [max 1 (m-k)..min k (m-1)]]

-- 2ª solución
-- ===========

zigZag2 :: Int -> Matrix Int
zigZag2 n = fromList n n (elementosZigZag2 n)

elementosZigZag2 :: Int -> [Int]
elementosZigZag2 n =
  map snd (sort (zip (ordenZigZag2 n) [0..]))

ordenZigZag2 :: Int -> [(Int,Int)]
ordenZigZag2 n = sortBy comp [(x,y) | x <- [1..n], y <- [1..n]]
    where comp (x1,y1) (x2,y2) | x1+y1 < x2+y2 = LT
                               | x1+y1 > x2+y2 = GT
                               | even (x1+y1)  = compare y1 y2
                               | otherwise     = compare x1 x2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_zigZag :: Positive Int -> Bool
prop_zigZag (Positive n) =
  zigZag1 n == zigZag2 n

-- La comprobación es
--    λ> quickCheck prop_zigZag
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (zigZag1 5000)
--    25000000
--    (2.57 secs, 1,800,683,952 bytes)
--    λ> length (zigZag2 5000)
--    25000000
--    (2.20 secs, 1,800,683,952 bytes)
--
--    λ> maximum (zigZag1 1100)
--    1209999
--    (2.12 secs, 1,840,095,864 bytes)
--    λ> maximum (zigZag2 1100)
--    1209999
--    (21.27 secs, 11,661,088,256 bytes)

{-
-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    elems (zigZag 3)
    == [0,1,5, 2,4,6, 3,7,8] &&
    elems (zigZag 4)
    == [0,1,5,6, 2,4,7,12, 3,8,11,13, 9,10,14,15] &&
    elems (zigZag 5)
    == [0,1,5,6,14, 2,4,7,13,15, 3,8,12,16,21, 9,11,17,20,22, 10,18,19,23,24]
    where zigZag = f

-- ---------------------------------------------------------------------
-- § Otras soluciones                                                 --
-- ---------------------------------------------------------------------

-- JoseJuan
-- ========

zigZagA1 :: (Num i, Enum i, Ix i) => i -> Array (i, i) i
zigZagA1 n = array bounds $ zip secundariasFlipadas [0..]
  where bounds              = ((1, 1), (n, n))
        secundarias         = diagonalesSecundarias $ fromBounds bounds
        secundariasFlipadas = concat
                            $ zipWith ($) (cycle [reverse, id]) secundarias

-- Construye una matriz cuyos elementos son sus índices
fromBounds :: (Enum i, Ix i) => ((i, i), (i, i)) -> Array (i, i) (i, i)
fromBounds (a@(x1, y1), b@(x2, y2)) =
    listArray (a,b) [(x, y) | x <- [x1..x2], y <- [y1..y2]]

diagonalesSecundarias :: (Enum i, Num i, Ix i) => Array (i, i) a -> [[a]]
diagonalesSecundarias v =
    [[v!(ax+x+y, ay-y) | y <- [-min (by-ay) x .. -max 0 (x-bx+ax)]]
     | x <- [0 .. bx-ax+by-ay]]
    where ((ax, ay), (bx, by)) = bounds v

-- Eficiencia
--    λ> take 15 (elems (zigZagA1 1000))
--    [0,1,5,6,14,15,27,28,44,45,65,66,90,91,119]
--    (5.74 secs, 698076448 bytes)

-- Jesús Navas
-- ===========

-- import Data.Array

zigZagA2 :: Int -> Array (Int,Int) Int
zigZagA2 n = array t (zip indices [0..n^2-1])
    where t       = ((1,1),(n,n))
          matriz  = listArray t (range t)
          indices = concat $ alternar $ diagonalesSecundariasA2 matriz

alternar :: [[a]] -> [[a]]
alternar []          = []
alternar [xs]        = [reverse xs]
alternar (xs:ys:xss) = reverse xs : ys : alternar xss

diagonalesSecundariasA2 :: Array (Int,Int) a -> [[a]]
diagonalesSecundariasA2 p =
    [[p!(i,j) | (i,j) <- extension ij] | ij <- laterales]
    where (_,(m,n))       = bounds p
          laterales       = [(1,j) | j <- [1..n-1]] ++ [(i,n) | i <- [1..m]]
          extension (i,j) = [(i+k,j-k) | k <- [0..min (m-i) (j-1)]]

-- Diego
-- =====

-- import Data.Array

zigZagA3 :: Int -> Array (Int,Int) Int
zigZagA3 n = array ((1,1),(n,n)) $
    zip [(i,d-i) | d <- [2..2*n],
                   let ix = [max 1 (d-n)..min n (d-1)],
                   i <- (if odd d then ix else reverse ix)] [0..n^2]

-- Eficiencia:
--    λ> take 15 (elems (zigZagA3 1000))
--    [0,1,5,6,14,15,27,28,44,45,65,66,90,91,119]
--    (3.32 secs, 607609808 bytes)

-- Jesús Navas
-- ===========

zigZagA4 :: Int -> Array (Int,Int) Int
zigZagA4 n =
    array t (zip (filter (\x-> inRange t x) $ concat $ alternar $ pares (2*n)) [0..n^2-1])
       where t = ((1,1),(n,n))
             alternar []          = []
             alternar [xs]        = [xs]
             alternar (xs:ys:xss) = xs : reverse ys : alternar xss

pares :: Int -> [[(Int,Int)]]
pares y = [[(n-x,x) | x <- [1..n-1]] | n <- [2..y]]

-- ---------------------------------------------------------------------
-- § Otras soluciones (15-16)                                         --
-- ---------------------------------------------------------------------

-- 1. erisancha
-- ============

zigZagB1 :: Int -> Array (Int,Int) Int
zigZagB1 n = listArray ((1,1),(n,n)) (elems' n)

elems' :: (Enum t, Num t, Ord t) => t -> [t]
elems' n = [snd x | x <- elementos n]

elementos :: (Enum t, Num t, Ord t) => t -> [((t, t), t)]
elementos n =
    sort [((x,y),i) | ((x,y),i) <- zip ((1,1):(concat (lista (1,2) n)))
                                       [0..n^2]]

lista :: (Num a, Ord a) => (a, a) -> a -> [[(a, a)]]
lista (x,y) n | x /= y = para (x,y) : lista (muevete (y,x) n) n
              | x == y = [[(x,y)]]

para :: (Num a, Ord a) => (a, a) -> [(a, a)]
para (x,y) | x < y     = takeWhile (<=(y,x)) (avanza (x,y))
           | x > y     = takeWhile (>=(y,x)) (retrocede (x,y))
           | otherwise = [(x,y)]

avanza :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
avanza (x,y) = (x,y) : avanza (x+1,y-1)

retrocede :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
retrocede (x,y) = (x,y) : retrocede (x-1,y+1)

muevete :: (Num a, Ord a) => (a, a) -> a -> (a, a)
muevete (x,y) n | x < y && y /= n = (x,y+1)
                | x < y && y == n = (x+1,y)
                | x > y && x /= n = (x+1,y)
                | x > y && x == n = (x,y+1)
                | x == y          = (x,y)

-- 2. carruirui3
-- =============

zigZagB2 :: Int -> Array (Int,Int) Int
zigZagB2 n =
    array ((1,1),(n,n)) [((i,j), zz n (i,j)) | i <- [1..n], j <- [1..n]]

-- zz recorre la lista en sentido inverso, acumulando el valor
zz :: Int -> (Int, Int) -> Int
zz _ (1,1) = 0
zz n (1,j) | even j     = 1   + zz n (1,   j-1)
           | otherwise  = j-1 + zz n (j,   1)
zz n (i,1) | odd i      = 1   + zz n (i-1, 1)
           | otherwise  = i-1 + zz n (1,   i)
zz n (i,j) | i+j > n+1  = n^2 - 1 - zz n (n+1-i,n+1-j)
           | even (i+j) = j-1 + zz n (i+j-1, 1)
           | otherwise  = i-1 + zz n (1,     i+j-1)

-- 3. abrdelrod
-- ============

separa :: [Int] -> [Int] -> [[Int]]
separa _ [] = []
separa xs (y:ys) = take y xs : separa (drop y xs) ys

reverseImpares :: [[Int]] -> [[Int]]
reverseImpares = map f.zip [1..]
  where f (n,xs) | even n = xs
                 | otherwise = reverse xs

diagonales :: Int -> [([Int],(Int,Int))]
diagonales n = zip (reverseImpares (separa [0..n^2-1] ys)) zs
   where ys = [1..n] ++ [n-1,n-2..1]
         zs = zip (replicate n 1 ++ [2..n]) ([1..n] ++ replicate (n-1) n)

zigZagB3 :: Int -> Array (Int,Int) Int
zigZagB3 n =
    listArray ((1,1),(n,n)) [f i j | i <- [1..n], j<- [1..n]]
    where f i j = let (a,_) = snd xs in fst xs !! abs (a-i)
              where xs = head [ys | ys <- diagonales n,
                                    let (a,b) = snd ys,
                                    a+b == i+j]

-- 4. Chema
-- ========

zigZagB4 :: Int -> Array (Int,Int) Int
zigZagB4 n = listArray ((1,1),(n,n)) $ concat (zipWith (++) xss yss)
    where xss = lineas $ inicial n
          yss = espejado n xss

-- Primera línea de la matriz
inicial :: Int -> [Int]
inicial n = take n $ scanl (+) 0 $ concat [[1,i]|i<-[4,8..]]

-- Creación de la parte superior de la matriz, hasta la diagonal
lineas :: [Int] -> [[Int]]
lineas xs = aux xs unos
    where unos = concat (repeat [1,-1])
          aux (x:xs) us =  (x:xs) : aux (zipWith (+) xs us) (tail us)
          aux _ _ = []

-- Construcción de la parte inferior de la matriz a partir de la superior
espejado :: Int -> [[Int]] -> [[Int]]
espejado n xss = reverse [(tail . reverse . map ((n*n-1)-)) xs | xs <- xss]
-}
