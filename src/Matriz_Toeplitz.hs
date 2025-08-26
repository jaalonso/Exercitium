-- Matriz_Toeplitz.hs
-- Matrices de Toepliz
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-Mayo-2014 (Revisión de 26-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una [matriz de Toeplitz](https://tinyurl.com/zqzokbm) es una matriz
-- cuadrada que es constante a lo largo de las diagonales paralelas a la
-- diagonal principal. Por ejemplo,
--    |2 5 1 6|       |2 5 1 6|
--    |4 2 5 1|       |4 2 6 1|
--    |7 4 2 5|       |7 4 2 5|
--    |9 7 4 2|       |9 7 4 2|
-- la primera es una matriz de Toeplitz y la segunda no lo es.
--
-- Las anteriores matrices se pueden definir por
--    ej1, ej2 :: Array (Int,Int) Int
--    ej1 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,5,1,7,4,2,5,9,7,4,2]
--    ej2 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,6,1,7,4,2,5,9,7,4,2]
--
-- Definir la función
--    esToeplitz :: Eq a => Array (Int,Int) a -> Bool
-- tal que (esToeplitz p) se verifica si la matriz p es de Toeplitz. Por
-- ejemplo,
--    esToeplitz ej1  ==  True
--    esToeplitz ej2  ==  False
-- ---------------------------------------------------------------------

module Matriz_Toeplitz where

import Data.Array (Array, (!), array, bounds, listArray)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

ej1, ej2 :: Array (Int,Int) Int
ej1 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,5,1,7,4,2,5,9,7,4,2]
ej2 = listArray ((1,1),(4,4)) [2,5,1,6,4,2,6,1,7,4,2,5,9,7,4,2]

-- 1ª solución
-- ===========

esToeplitz1 :: Eq a => Array (Int,Int) a -> Bool
esToeplitz1 p =
  esCuadrada p &&
  all todosIguales (diagonalesPrincipales p)

-- (esCuadrada p) se verifica si la matriz p es cuadrada. Por ejemplo,
--    esCuadrada (listArray ((1,1),(4,4)) [1..])  ==  True
--    esCuadrada (listArray ((1,1),(3,4)) [1..])  ==  False
esCuadrada :: Eq a => Array (Int,Int) a -> Bool
esCuadrada p = m == n
  where (_,(m,n)) = bounds p

-- (diagonalesPrincipales p) es la lista de las diagonales principales
-- de p. Por ejemplo,
--    λ> diagonalesPrincipales ej1
--    [[2,2,2,2],[5,5,5],[1,1],[6],[2,2,2,2],[4,4,4],[7,7],[9]]
--    λ> diagonalesPrincipales ej2
--    [[2,2,2,2],[5,6,5],[1,1],[6],[2,2,2,2],[4,4,4],[7,7],[9]]
diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales p =
  [[p ! i |i <- is] | is <- posicionesDiagonalesPrincipales m n]
  where (_,(m,n)) = bounds p

-- (posicionesDiagonalesPrincipales m n) es la lista de las
-- posiciones de las diagonales principales de una matriz con m filas y
-- n columnas. Por ejemplo,
--   λ> mapM_ print (posicionesDiagonalesPrincipales 3 4)
--   [(3,1)]
--   [(2,1),(3,2)]
--   [(1,1),(2,2),(3,3)]
--   [(1,2),(2,3),(3,4)]
--   [(1,3),(2,4)]
--   [(1,4)]
--   λ> mapM_ print (posicionesDiagonalesPrincipales 4 4)
--   [(4,1)]
--   [(3,1),(4,2)]
--   [(2,1),(3,2),(4,3)]
--   [(1,1),(2,2),(3,3),(4,4)]
--   [(1,2),(2,3),(3,4)]
--   [(1,3),(2,4)]
--   [(1,4)]
--   λ> mapM_ print (posicionesDiagonalesPrincipales 4 3)
--   [(4,1)]
--   [(3,1),(4,2)]
--   [(2,1),(3,2),(4,3)]
--   [(1,1),(2,2),(3,3)]
--   [(1,2),(2,3)]
--   [(1,3)]
posicionesDiagonalesPrincipales :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales m n =
  [zip [i..m] [1..n] | i <- [m,m-1..1]] ++
  [zip [1..m] [j..n] | j <- [2..n]]

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [5,5,5]  ==  True
--    todosIguales [5,4,5]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales []     = True
todosIguales (x:xs) = all (== x) xs

-- 2ª solución
-- ===========

esToeplitz2 :: Eq a => Array (Int,Int) a -> Bool
esToeplitz2 p = m == n &&
                and [p!(i,j) == p!(i+1,j+1) |
                     i <- [1..n-1], j <- [1..n-1]]
  where (_,(m,n)) = bounds p

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Array (Int,Int) Int -> Bool) -> Spec
specG esToeplitz = do
  it "e1" $
    esToeplitz ej1 `shouldBe` True
  it "e2" $
    esToeplitz ej2 `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG esToeplitz1
  describe "def. 2" $ specG esToeplitz2

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

newtype Matriz2 = M (Array (Int,Int) Int)
  deriving Show

-- Generador de matrices arbitrarias. Por ejemplo,
--    λ> generate matrizArbitraria
--    M (array ((1,1),(3,4))
--             [((1,1),18),((1,2),6), ((1,3),-23),((1,4),-13),
--              ((2,1),-2),((2,2),22),((2,3),-25),((2,4),-5),
--              ((3,1),2), ((3,2),16),((3,3),-15),((3,4),7)])
matrizArbitraria :: Gen Matriz2
matrizArbitraria = do
  n  <- chooseInt (1,5)
  xs <- vectorOf (n*n) arbitrary
  return (M (listArray ((1,1),(n,n)) xs))

-- Generador de matrices de Toeplitz arbitrarias. Por ejemplo,
--    λ> generate matrizToeplitzArbitraria
--    M (array ((1,1),(3,3)) [((1,1),-28),((1,2), 28),((1,3),  9),
--                            ((2,1),  6),((2,2),-28),((2,3), 28),
--                            ((3,1),-29),((3,2),  6),((3,3),-28)])
matrizToeplitzArbitraria :: Gen Matriz2
matrizToeplitzArbitraria = do
  n <- chooseInt (1, 5)
  primeraFila <- vectorOf n arbitrary
  primeraColumna <- vectorOf (n-1) arbitrary
  let xs = [((i,j), if i <= j
                    then primeraFila !! (j-i)
                    else primeraColumna !! (i-j-1))
           | i <- [1..n], j <- [1..n]]
  return (M (array ((1,1),(n,n)) xs))

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz2 where
  arbitrary = frequency
    [ (1, matrizToeplitzArbitraria)  -- 25% matrices de Toeplitz
    , (3, matrizArbitraria)          -- 75% matrices aleatorias
    ]

-- La propiedad es
prop_esToeplitz :: Matriz2 -> Bool
prop_esToeplitz (M p) =
  esToeplitz1 p == esToeplitz2 p

-- La comprobación es
--    λ> quickCheck prop_esToeplitz
--    +++ OK, passed 100 tests.

-- La propiedad para que indique el porcentaje de matrices de Toeplitz
-- generadas.
prop_esToeplitz2 :: Matriz2 -> Property
prop_esToeplitz2 (M p) =
  collect resultado1 $ resultado1 == esToeplitz2 p
  where resultado1 = esToeplitz1 p

-- La comprobación es
--    λ> quickCheck prop_esToeplitz2
--    +++ OK, passed 100 tests:
--    58% False
--    42% True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esToeplitz1 (listArray ((1,1),(2*10^3,2*10^3)) (repeat 1))
--    True
--    (2.26 secs, 2,211,553,888 bytes)
--    λ> esToeplitz2 (listArray ((1,1),(2*10^3,2*10^3)) (repeat 1))
--    True
--    (4.26 secs, 3,421,651,032 bytes)
