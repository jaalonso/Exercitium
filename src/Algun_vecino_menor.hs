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

type Pos = (Int,Int)

-- 1ª solución
-- ===========

algunoMenor1 :: Matriz -> [Int]
algunoMenor1 a =
  [a!p| p <- indices a,
        any (< a!p) (vecinos1 a p)]

-- (vecinos q p) es la lista de los vecinos en la matriz a de la
-- posición p. Por ejemplo,
--    vecinos1 ej (2,2)  ==  [9,4,6,8,7,4,2,5]
--    vecinos1 ej (1,1)  ==  [4,8,1]
vecinos1 :: Matriz -> Pos -> [Int]
vecinos1 a p =
  [a!p' | p' <- posicionesVecinos1 a p]

-- (posicionesVecinos a p) es la lista de las posiciones de los
-- vecino de p en la matriz a. Por ejemplo,
--    λ> posicionesVecinos1 3 3 (2,2)
--    [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]
--    λ> posicionesVecinos1 3 3 (1,1)
--    [(1,2),(2,1),(2,2)]
posicionesVecinos1 :: Matriz -> Pos -> [Pos]
posicionesVecinos1 a (i,j) =
  [(i+di,j+dj) | (di,dj) <- [(-1,-1),(-1,0),(-1,1),
                             ( 0,-1),       ( 0,1),
                             ( 1,-1),( 1,0),( 1,1)],
                 inRange (bounds a) (i+di,j+dj)]

-- 2ª solución
-- ===========

algunoMenor2 :: Matriz -> [Int]
algunoMenor2 a =
  [a!p | p <- indices a,
         any (<a!p) (vecinos2 p)]
  where
    vecinos2 p =
      [a!p' | p' <- posicionesVecinos2 p]
    posicionesVecinos2 (i,j) =
      [(i+di,j+dj) | (di,dj) <- [(-1,-1),(-1,0),(-1,1),
                                 ( 0,-1),       ( 0,1),
                                 ( 1,-1),( 1,0),( 1,1)],
                     inRange (bounds a) (i+di,j+dj)]

-- 3ª solución
-- ===========

algunoMenor3 :: Matriz -> [Int]
algunoMenor3 a =
  [a!p | p <- indices a,
         any (<a!p) (vecinos3 p)]
  where
    vecinos3 p =
      [a!p' | p' <- posicionesVecinos3 p]
    posicionesVecinos3 (i,j) =
      [(i',j') | i' <- [i-1..i+1],
                 j' <- [j-1..j+1],
                 (i',j') /= (i,j),
                 inRange (bounds a) (i',j')]

-- 4ª solución
-- ===========

algunoMenor4 :: Matriz -> [Int]
algunoMenor4 a =
  [a!p | p <- indices a,
         any (<a!p) (vecinos4 p)]
  where
    vecinos4 p =
      [a!p' | p' <- posicionesVecinos4 p]
    posicionesVecinos4 (i,j) =
      [(i',j') | i' <- [max 1 (i-1)..min m (i+1)],
                 j' <- [max 1 (j-1)..min n (j+1)],
                 (i',j') /= (i,j)]
      where (_,(m,n)) = bounds a


-- 5ª solución
-- ===========

algunoMenor5 :: Matriz -> [Int]
algunoMenor5 a =
  [a!p | p <- indices a,
         any (<a!p) (vecinos5 p)]
  where
    vecinos5 p =
      [a!p' | p' <- posicionesVecinos5 p]
    posicionesVecinos5 (i,j) =
      [(i-1,j-1) | i > 1, j > 1] ++
      [(i-1,j)   | i > 1]        ++
      [(i-1,j+1) | i > 1, j < n] ++
      [(i,j-1)   | j > 1]        ++
      [(i,j+1)   | j < n]        ++
      [(i+1,j-1) | i < m, j > 1] ++
      [(i+1,j)   | i < m]        ++
      [(i+1,j+1) | i < m, j < n]
      where (_,(m,n)) = bounds a

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
--    (2.20 secs, 1,350,075,240 bytes)
--    λ> maximum (algunoMenor2 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.24 secs, 1,373,139,968 bytes)
--    λ> maximum (algunoMenor3 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.08 secs, 1,200,734,112 bytes)
--    λ> maximum (algunoMenor4 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (2.76 secs, 1,287,653,136 bytes)
--    λ> maximum (algunoMenor5 (listArray ((1,1),(600,800)) [0..]))
--    479999
--    (1.67 secs, 953,937,600 bytes)
