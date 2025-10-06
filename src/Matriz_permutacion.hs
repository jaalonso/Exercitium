-- Matriz_permutacion.hs
-- Matriz permutación.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-Julio-2014 (actualizado 6-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una matriz permutación es una matriz cuadrada con todos sus elementos
-- iguales a 0, excepto uno cualquiera por cada fila y columna, el cual
-- debe ser igual a 1.
--
-- En este ejercicio se usará el tipo de las matrices definido por
--    type Matriz a = Array (Int,Int) a
-- y los siguientes ejemplos de matrices
--    q1, q2, q3,q4 :: Matriz Int
--    q1 = array ((1,1),(2,2)) [((1,1),1),((1,2),0),((2,1),0),((2,2),1)]
--    q2 = array ((1,1),(2,2)) [((1,1),0),((1,2),1),((2,1),0),((2,2),1)]
--    q3 = array ((1,1),(2,2)) [((1,1),3),((1,2),0),((2,1),0),((2,2),1)]
--    q4 = array ((1,1),(2,2)) [((1,1),1),((1,2),3),((2,1),0),((2,2),1)]
--
-- Definir la función
--    esMatrizPermutacion :: Num a => Matriz a -> Bool
-- tal que (esMatrizPermutacion p) se verifica si p es una matriz
-- permutación. Por ejemplo.
--    esMatrizPermutacion q1  ==  True
--    esMatrizPermutacion q2  ==  False
--    esMatrizPermutacion q3  ==  False
-- ---------------------------------------------------------------------

module Matriz_permutacion where

import Data.Array
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Matriz a = Array (Int,Int) a

q1, q2, q3, q4 :: Matriz Int
q1 = array ((1,1),(2,2)) [((1,1),1),((1,2),0),((2,1),0),((2,2),1)]
q2 = array ((1,1),(2,2)) [((1,1),0),((1,2),1),((2,1),0),((2,2),1)]
q3 = array ((1,1),(2,2)) [((1,1),3),((1,2),0),((2,1),0),((2,2),1)]
q4 = array ((1,1),(2,2)) [((1,1),1),((1,2),3),((2,1),0),((2,2),1)]

-- 1ª solución
-- ===========

esMatrizPermutacion1 :: (Num a, Eq a) => Matriz a -> Bool
esMatrizPermutacion1 p =
  all esListaUnitaria (filas p) &&
  all esListaUnitaria (columnas p)

-- (filas p) es la lista de las filas de la matriz p. Por ejemplo,
--    filas q1 == [[1,0],[0,1]]
--    filas q2 == [[0,1],[0,1]]
--    filas q3 == [[3,0],[0,1]]
--    filas q4 == [[1,3],[0,1]]
filas :: (Num a, Eq a) => Matriz a -> [[a]]
filas p =
  [[p!(i,j) | j <- [1..n]] | i <- [1..n]]
  where (_,(n,_)) = bounds p

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo,
--    columnas q1 == [[1,0],[0,1]]
--    columnas q2 == [[0,0],[1,1]]
--    columnas q3 == [[3,0],[0,1]]
--    columnas q4 == [[1,0],[3,1]]
columnas :: (Num a, Eq a) => Matriz a -> [[a]]
columnas p =
  [[p!(i,j) | i <- [1..n]] | j <- [1..n]]
  where (_,(n,_)) = bounds p

-- (esListaUnitaria xs) se verifica si xs tiene un 1 y los restantes
-- elementos son 0. Por ejemplo,
--    esListaUnitaria [0,1,0,0]  ==  True
--    esListaUnitaria [0,1,0,1]  ==  False
--    esListaUnitaria [0,2,0,0]  ==  False
esListaUnitaria :: (Num a, Eq a) => [a] -> Bool
esListaUnitaria xs =
  [x | x <- xs, x /= 0] == [1]

-- 2ª solución
-- ===========

esMatrizPermutacion2 :: (Num a, Eq a) => Matriz a -> Bool
esMatrizPermutacion2 p =
  all esListaUnitaria (filas p ++ columnas p)

-- 3ª solución
-- ===========

esMatrizPermutacion3 :: (Num a, Eq a) => Matriz a -> Bool
esMatrizPermutacion3 p =
    and [esListaUnitaria [p!(i,j) | i <- [1..n]] | j <- [1..n]] &&
    and [esListaUnitaria [p!(i,j) | j <- [1..n]] | i <- [1..n]]
    where (_,(n,_)) = bounds p

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Matriz Int -> Bool) -> Spec
specG esMatrizPermutacion = do
  it "e1" $
    esMatrizPermutacion q1 `shouldBe` True
  it "e2" $
    esMatrizPermutacion q2 `shouldBe` False
  it "e3" $
    esMatrizPermutacion q3 `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esMatrizPermutacion1
  describe "def. 2" $ specG esMatrizPermutacion2
  describe "def. 3" $ specG esMatrizPermutacion3

-- La verificación es
--    λ> verifica
--    9 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

newtype Matriz2 = M (Array (Int,Int) Int)
  deriving Show

-- (matrizArbitraria n) es un  generador de matrices cuadradas
-- arbitrarias de orden nxn. Por ejemplo,
--    λ> generate (matrizArbitraria 3)
--    M (array ((1,1),(3,3)) [((1,1),-8), ((1,2),3),  ((1,3),-21),
--                            ((2,1),-17),((2,2),-24),((2,3),30),
--                            ((3,1),-29),((3,2),17), ((3,3),-28)])
matrizArbitraria :: Int -> Gen Matriz2
matrizArbitraria n = do
  xs <- vectorOf (n*n) arbitrary
  return (M (listArray ((1,1),(n,n)) xs))

-- (permutacionAfila xs i) es la lista de los elementos de la fila
-- i-ésima de la matriz permutación correspondiente a la permutación xs
-- de los números peimeros números. Por ejemplo,
--    permutacionAfila [3,1,2] 1 == [0,1,0]
--    permutacionAfila [3,1,2] 2 == [0,0,1]
--    permutacionAfila [3,1,2] 3 == [1,0,0]
permutacionAfila :: [Int] -> Int -> [Int]
permutacionAfila xs i =
  map f xs
  where f x | x == i    = 1
            | otherwise = 0

-- (permutacionAmatriz xs) es la matriz permutación correspondiente a la
-- permutación xs de los números peimeros números. Por ejemplo,
--    λ> permutacionAmatriz [3,1,2]
--    array ((1,1),(3,3)) [((1,1),0),((1,2),1),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),1),
--                         ((3,1),1),((3,2),0),((3,3),0)]
permutacionAmatriz :: [Int] -> Matriz Int
permutacionAmatriz xs =
  listArray ((1,1),(n,n)) (concat [permutacionAfila xs i | i <- [1..n]])
  where n = length xs

-- (permutacionArbitraria n) es un generador de permutaciones de los
-- números [1..n]. Por ejemplo,
--    λ> generate (permutacionArbitraria 5)
--    [3,5,2,4,1]
permutacionArbitraria :: Int -> Gen [Int]
permutacionArbitraria n =
  shuffle [1..n]

-- (matrizPermutacionArbitraria n) es un  generador de matrices
-- permutació arbitrarias de orden nxn. Por ejemplo,
--    λ> generate (matrizPermutacionArbitraria 3)
--    M (array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                            ((2,1),0),((2,2),0),((2,3),1),
--                            ((3,1),0),((3,2),1),((3,3),0)])
matrizPermutacionArbitraria :: Int -> Gen Matriz2
matrizPermutacionArbitraria n = do
  xs <- permutacionArbitraria n
  return (M (permutacionAmatriz xs))

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz2 where
  arbitrary = sized $ \n -> do
    frequency
      [ (3, matrizPermutacionArbitraria n)  -- 75% matrices permutación
      , (1, matrizArbitraria n)             -- 25% matrices aleatorias
      ]

-- La propiedad es
prop_esMatrizPermutacion :: Matriz2 -> Bool
prop_esMatrizPermutacion (M p) =
  all (== esMatrizPermutacion1 p)
      [esMatrizPermutacion2 p,
       esMatrizPermutacion2 p]

-- La comprobación es
--    λ> quickCheck prop_esMatrizPermutacion
--    +++ OK, passed 100 tests.

-- La propiedad para que indique el porcentaje de matrices permutación
-- generadas.
prop_esMatrizPermutacion2 :: Matriz2 -> Property
prop_esMatrizPermutacion2 (M p) =
  collect r $ esMatrizPermutacion2 p == r && esMatrizPermutacion3 p == r
  where r = esMatrizPermutacion1 p

-- La comprobación es
--    λ> quickCheck prop_esMatrizPermutacion2
--    +++ OK, passed 100 tests:
--    76% True
--    24% False

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> (M ej) <- generate (matrizPermutacionArbitraria 1000)
--    λ> esMatrizPermutacion1 ej
--    True
--    (1.72 secs, 1,251,562,208 bytes)
--    λ> esMatrizPermutacion2 ej
--    True
--    (1.19 secs, 978,158,784 bytes)
--    λ> esMatrizPermutacion3 ej
--    True
--    (1.16 secs, 978,454,728 bytes)
