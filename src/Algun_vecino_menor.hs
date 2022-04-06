-- Algun_vecino_menor.hs
-- Elementos de una matriz con algún vecino menor
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices puede representarse mediante tablas cuyos índices son
-- pares de números naturales. Su tipo se define por
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- se define por
--    ej :: Matriz
--    ej = listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4]
--
-- Los vecinos de un elemento son los que están a un paso en la misma
-- fila, columna o diagonal. Por ejemplo, en la matriz anterior, el 1
-- tiene 8 vecinos (el 9, 4, 6, 8, 7, 4, 2 y 5) pero el 9 sólo tiene 3
-- vecinos (el 4, 8 y 1).
--
-- Definir la función
--    algunoMenor :: Matriz -> [Int]
-- tal que (algunoMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,
--    algunoMenor ej == [9,4,6,5,8,7,4,2,5,4]
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Algun_vecino_menor where

import Data.Array (Array, (!), bounds, indices, inRange, listArray)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, chooseInt, quickCheck,
                        vectorOf)

type Matriz = Array (Int,Int) Int

ej :: Matriz
ej = listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4]

-- 1ª solución
-- ===========

algunoMenor1 :: Matriz -> [Int]
algunoMenor1 p =
  [p!(i,j) | (i,j) <- indices p,
             not (null (vecinosMenores1 p (i,j)))]

-- (vecinosMenores1 p (i,j)) es la lista de los vecinos en la matriz p
-- de la posición (i,j) que son menores que el elemento en dicha
-- posición. Por ejemplo,
--    vecinosMenores1 ej (2,3)  ==  [4,6,5,1,3,2,5,4]
--    vecinosMenores1 ej (2,2)  ==  []
vecinosMenores1 :: Matriz -> (Int,Int) -> [Int]
vecinosMenores1 p (i,j) =
  [p!(a,b) | (a,b) <- posicionesVecinos1 m n (i,j),
             p!(a,b) < x]
  where (_,(m,n)) = bounds p
        x         = p!(i,j)

-- (vecinos1 p (i,j)) es la lista de los vecinos en la matriz p de la
-- posición (i,j). Por ejemplo,
vecinos1 :: Matriz -> (Int,Int) -> [Int]
vecinos1 p (i,j) =
  [p!(a,b) | (a,b) <- posicionesVecinos1 m n (i,j)]
  where (_,(m,n)) = bounds p

-- (posicionesVecinos5 m n (i,j)) es la lista de las posiciones de los
-- vecino de (i,j) en una matriz con m filas y n columnas. Por ejemplo,
--    λ> posicionesVecinos2 3 3 (2,2)
--    [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]
--    λ> posicionesVecinos2 3 3 (1,1)
--    [(1,2),(2,1),(2,2)]
posicionesVecinos1 :: Int -> Int -> (Int,Int) -> [(Int,Int)]
posicionesVecinos1 m n (i,j) =
  [(i+a,j+b) | (a,b) <- [(-1,-1),(-1,0),(-1,1),
                         ( 0,-1),       ( 0,1),
                         ( 1,-1),( 1,0),( 1,1)],
               inRange ((1,1),(m,n)) (i+a,j+b)]

-- 2ª solución
-- ===========

algunoMenor2 :: Matriz -> [Int]
algunoMenor2 p =
  [p!(i,j) | (i,j) <- indices p,
             not (null (vecinosMenores2 (i,j)))]
  where
    vecinosMenores2 (i,j) =
      [p!(a,b) | (a,b) <- posicionesVecinos2 (i,j),
                  p!(a,b) < x]
      where x = p!(i,j)
    posicionesVecinos2 (i,j) =
      [(i+a,j+b) | (a,b) <- [(-1,-1),(-1,0),(-1,1),
                             ( 0,-1),       ( 0,1),
                             ( 1,-1),( 1,0),( 1,1)],
                   inRange (bounds p) (i+a,j+b)]

-- 3ª solución
-- ===========

algunoMenor3 :: Matriz -> [Int]
algunoMenor3 p =
  [p!(i,j) | (i,j) <- indices p,
             not (null (vecinosMenores3 (i,j)))]
  where
    vecinosMenores3 (i,j) =
      [p!(a,b) | (a,b) <- posicionesVecinos3 (i,j),
                  p!(a,b) < x]
      where x = p!(i,j)
    posicionesVecinos3 (i,j) =
      [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
               b <- [max 1 (j-1)..min n (j+1)],
               (a,b) /= (i,j)]
      where (_,(m,n)) = bounds p

-- 4ª solución
-- ===========

algunoMenor4 :: Matriz -> [Int]
algunoMenor4 p =
  [p!(i,j) | (i,j) <- indices p,
             not (null (vecinosMenores4 (i,j)))]
  where
    vecinosMenores4 (i,j) =
      [p!(a,b) | (a,b) <- posicionesVecinos4 (i,j),
                  p!(a,b) < x]
      where x = p!(i,j)
    posicionesVecinos4 (i,j) =
      [(a,b) | a <- [i-1..i+1],
               b <- [j-1..j+1],
               (a,b) /= (i,j),
               inRange ((1,1),(m,n)) (a,b)]
      where (_,(m,n)) = bounds p

-- 5ª solución
-- ===========

algunoMenor5 :: Matriz -> [Int]
algunoMenor5 p =
  [p!(i,j) | (i,j) <- indices p,
             not (null (vecinosMenores5 (i,j)))]
  where
    vecinosMenores5 (i,j) =
      [p!(a,b) | (a,b) <- posicionesVecinos5 (i,j),
                  p!(a,b) < x]
      where x = p!(i,j)
    posicionesVecinos5 (i,j) =
      [(i-1,j-1) | i > 1, j > 1] ++
      [(i-1,j)   | i > 1]        ++
      [(i-1,j+1) | i > 1, j < n] ++
      [(i,j-1)   | j > 1]        ++
      [(i,j+1)   | j < n]        ++
      [(i+1,j-1) | i < m, j > 1] ++
      [(i+1,j)   | i < m]        ++
      [(i+1,j+1) | i < m, j < n]
      where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------

-- Comprobación de equivalencia
-- ============================

newtype Matriz2 = M Matriz
  deriving Show

-- Generador de matrices arbitrarias. Por ejemplo,
--    λ> generate matrizArbitraria
--    M (array ((1,1),(3,4))
--             [((1,1),18),((1,2),6), ((1,3),-23),((1,4),-13),
--              ((2,1),-2),((2,2),22),((2,3),-25),((2,4),-5),
--              ((3,1),2), ((3,2),16),((3,3),-15),((3,4),7)])
matrizArbitraria :: Gen Matriz2
matrizArbitraria = do
  m  <- chooseInt (1,10)
  n  <- chooseInt (1,10)
  xs <- vectorOf (m*n) arbitrary
  return (M (listArray ((1,1),(m,n)) xs))

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz2 where
  arbitrary = matrizArbitraria

-- La propiedad es
prop_algunoMenor :: Matriz2 -> Bool
prop_algunoMenor (M p) =
  all (== algunoMenor1 p)
      [algunoMenor2 p,
       algunoMenor3 p,
       algunoMenor4 p,
       algunoMenor5 p]

-- La comprobación es
--    λ> quickCheck prop_algunoMenor
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximum (algunoMenor1 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.01 secs, 1,749,514,416 bytes)
--    λ> maximum (algunoMenor2 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (1.71 secs, 1,653,395,008 bytes)
--    λ> maximum (algunoMenor3 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.09 secs, 1,567,908,176 bytes)
--    λ> maximum (algunoMenor4 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.03 secs, 1,604,019,704 bytes)
--    λ> maximum (algunoMenor5 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (1.39 secs, 1,234,192,640 bytes)
