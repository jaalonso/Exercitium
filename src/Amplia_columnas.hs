-- Amplia_columnas.hs
-- Ampliación de matrices por columnas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-marzo-2022
-- ---------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Las matrices enteras se pueden representar mediante tablas con
-- índices enteros:
--    type Matriz = Array (Int,Int) Int
--
-- Definir la función
--    ampliaColumnas :: Matriz -> Matriz -> Matriz
-- tal que (ampliaColumnas p q) es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representa
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera
--    |0 1|    |4 5 6|    |0 1 4 5 6|
--    |2 3|    |7 8 9|    |2 3 7 8 9|
-- En Haskell, se definen las dos primeras matrices se definen por
--    ej1 = listArray ((1,1),(2,2)) [0..3]
--    ej2 = listArray ((1,1),(2,3)) [4..9]
-- y el cálculo de la tercera es
--    λ> ampliaColumnas ej1 ej2
--    array ((1,1),(2,5)) [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
--                         ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
--    λ> elems (ampliaColumnas ej1 ej2)
--    [0,1,4,5,6,2,3,7,8,9]
-- ---------------------------------------------------------------------

module Amplia_columnas where

import Data.Array (Array, (!), array, bounds, elems, listArray)
import Data.Matrix (Matrix, (<|>), fromList, ncols, nrows, toList)
import Test.QuickCheck

type Matriz = Array (Int,Int) Int

ej1, ej2 :: Matriz
ej1 = listArray ((1,1),(2,2)) [0..3]
ej2 = listArray ((1,1),(2,3)) [4..9]

-- 1ª solución
-- ===========

ampliaColumnas1 :: Matriz -> Matriz -> Matriz
ampliaColumnas1 p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1)

-- 2ª solución
-- ===========

ampliaColumnas2 :: Matriz -> Matriz -> Matriz
ampliaColumnas2 p1 p2 =
  matriz (matrix p1 <|> matrix p2)

-- (matrix p) es la matriz p en el formato de Data.Matrix. Por ejemplo,
--    λ> ej1
--    array ((1,1),(2,2)) [((1,1),0),((1,2),1),((2,1),2),((2,2),3)]
--    λ> matrix ej1
--    ┌     ┐
--    │ 0 1 │
--    │ 2 3 │
--    └     ┘
--    λ> matrix (ampliaColumnas1 ej1 ej2)
--    ┌           ┐
--    │ 0 1 4 5 6 │
--    │ 2 3 7 8 9 │
--    └           ┘
matrix :: Matriz -> Matrix Int
matrix p = fromList m n (elems p)
  where (_,(m,n)) = bounds p

-- (matriz p) es la matriz p en el formato de Data.Array. Por ejemplo,
--    λ> matriz (fromList 2 3 [1..])
--    array ((1,1),(2,3)) [((1,1),1),((1,2),2),((1,3),3),((2,1),4),((2,2),5),((2,3),6)]
matriz :: Matrix Int -> Matriz
matriz p = listArray ((1,1),(nrows p,ncols p)) (toList p)

-- Comprobación de equivalencia
-- ============================

data ParMatrices = P Matriz Matriz
  deriving Show

-- parMatricesArbitrario es un generador de pares de matrices con el
-- mismo número de filas.
parMatricesArbitrario :: Gen ParMatrices
parMatricesArbitrario = do
  m  <- arbitrary `suchThat` (> 0)
  n1 <- arbitrary `suchThat` (> 0)
  n2 <- arbitrary `suchThat` (> 0)
  xs <- vector (m * n1)
  ys <- vector (m * n2)
  return (P (listArray ((1,1),(m,n1)) xs)
            (listArray ((1,1),(m,n2)) ys))

-- ParMatrices es una subclase de Arbitrary
instance Arbitrary ParMatrices where
  arbitrary = parMatricesArbitrario

-- La propiedad es
prop_ampliaColumna :: ParMatrices -> Bool
prop_ampliaColumna (P p q) =
  ampliaColumnas1 p q == ampliaColumnas2 p q

-- La comprobación es
--    λ> quickCheck prop_ampliaColumna
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> let p = listArray ((1,1),(10^3,10^3)) [1..] in maximum (ampliaColumnas1 p p)
--    1000000
--    (2.04 secs, 1,562,652,704 bytes)
--    λ> let p = listArray ((1,1),(10^3,10^3)) [1..] in maximum (ampliaColumnas2 p p)
--    1000000
--    (0.69 secs, 738,508,624 bytes)
