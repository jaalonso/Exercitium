-- Maxima_suma_de_los_caminos_en_una_matriz.hs
-- Máxima suma de los caminos en una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-octubre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los caminos desde el extremo superior izquierdo (posición (1,1))
-- hasta el extremo inferior derecho (posición (3,4)) en la matriz
--    (  1  6 11  2 )
--    (  7 12  3  8 )
--    (  3  8  4  9 )
-- moviéndose en cada paso una casilla hacia la derecha o hacia abajo,
-- son los siguientes:
--    [1,6,11,2,8,9]
--    [1,6,11,3,8,9]
--    [1,6,12,3,8,9]
--    [1,7,12,3,8,9]
--    [1,6,11,3,4,9]
--    [1,6,12,3,4,9]
--    [1,7,12,3,4,9]
--    [1,6,12,8,4,9]
--    [1,7,12,8,4,9]
--    [1,7, 3,8,4,9]
-- La suma de los caminos son 37, 38, 39, 40, 34, 35, 36, 40, 41 y 32,
-- respectivamente. El camino de máxima suma es el penúltimo (1, 7, 12, 8,
-- 4, 9) que tiene una suma de 41.
--
-- Definir la función
--    maximaSuma :: Matrix Int -> Int
-- tal que (maximaSuma m) es el máximo de las sumas de los caminos en la
-- matriz m desde el extremo superior izquierdo hasta el extremo
-- inferior derecho, moviéndose en cada paso una casilla hacia abajo o
-- hacia la derecha. Por ejemplo,
--    λ> maximaSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    41
--    λ> maximaSuma (fromList 800 800 [1..])
--    766721999
-- ---------------------------------------------------------------------

module Maxima_suma_de_los_caminos_en_una_matriz where

import Data.Matrix (Matrix, (!), fromList, fromLists, matrix, nrows, ncols)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definicion de maximaSuma (con caminos1)
-- ==========================================

maximaSuma1 :: Matrix Int -> Int
maximaSuma1 =
  maximum . map sum . caminos1

caminos1 :: Matrix Int -> [[Int]]
caminos1 m =
  reverse (map reverse (caminos1Aux m (nf,nc)))
  where nf = nrows m
        nc = ncols m

-- (caminos1Aux m p) es la lista de los caminos invertidos en la matriz m
-- desde la posición (1,1) hasta la posición p. Por ejemplo,
caminos1Aux :: Matrix Int -> (Int,Int) -> [[Int]]
caminos1Aux m (1,1) = [[m!(1,1)]]
caminos1Aux m (1,j) = [[m!(1,k) | k <- [j,j-1..1]]]
caminos1Aux m (i,1) = [[m!(k,1) | k <- [i,i-1..1]]]
caminos1Aux m (i,j) = [m!(i,j) : xs
                      | xs <- caminos1Aux m (i,j-1) ++
                              caminos1Aux m (i-1,j)]

-- 2ª definición de maximaSuma (con caminos2)
-- ==========================================

maximaSuma2 :: Matrix Int -> Int
maximaSuma2 =
  maximum . map sum . caminos2

caminos2 :: Matrix Int -> [[Int]]
caminos2 m =
  map reverse (matrizCaminos m ! (nrows m, ncols m))

matrizCaminos :: Matrix Int -> Matrix [[Int]]
matrizCaminos m = q
  where
    q = matrix (nrows m) (ncols m) f
    f (1,y) = [[m!(1,z) | z <- [y,y-1..1]]]
    f (x,1) = [[m!(z,1) | z <- [x,x-1..1]]]
    f (x,y) = [m!(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]

-- 3ª definicion de maximaSuma (por recursión)
-- ===========================================

maximaSuma3 :: Matrix Int -> Int
maximaSuma3 m = maximaSuma3Aux m (nf,nc)
  where nf = nrows m
        nc = ncols m

-- (maximaSuma3Aux m p) calcula la suma máxima de un camino hasta la
-- posición p. Por ejemplo,
--    λ> maximaSuma3Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (3,4)
--    41
--    λ> maximaSuma3Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (3,3)
--    32
--    λ> maximaSuma3Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (2,4)
--    31
maximaSuma3Aux :: Matrix Int -> (Int,Int) -> Int
maximaSuma3Aux m (1,1) = m ! (1,1)
maximaSuma3Aux m (1,j) = maximaSuma3Aux m (1,j-1) + m ! (1,j)
maximaSuma3Aux m (i,1) = maximaSuma3Aux m (i-1,1) + m ! (i,1)
maximaSuma3Aux m (i,j) =
  max (maximaSuma3Aux m (i,j-1)) (maximaSuma3Aux m (i-1,j)) + m ! (i,j)

-- 4ª solución (mediante programación dinámica)
-- ============================================

maximaSuma4 :: Matrix Int -> Int
maximaSuma4 m = q ! (nf,nc)
  where nf = nrows m
        nc = ncols m
        q  = matrizMaximaSuma m

-- (matrizMaximaSuma m) es la matriz donde en cada posición p se
-- encuentra el máxima de las sumas de los caminos desde (1,1) a p en la
-- matriz m. Por ejemplo,
--    λ> matrizMaximaSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    (  1  7 18 20 )
--    (  8 20 23 31 )
--    ( 11 28 32 41 )
matrizMaximaSuma :: Matrix Int -> Matrix Int
matrizMaximaSuma m = q
  where nf = nrows m
        nc = ncols m
        q  = matrix nf nc f
          where  f (1,1) = m ! (1,1)
                 f (1,j) = q ! (1,j-1) + m ! (1,j)
                 f (i,1) = q ! (i-1,1) + m ! (i,1)
                 f (i,j) = max (q ! (i,j-1)) (q ! (i-1,j)) + m ! (i,j)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximaSuma1 (fromList 11 11 [1..])
--    1781
--    (3.88 secs, 1,525,812,680 bytes)
--    λ> maximaSuma2 (fromList 11 11 [1..])
--    1781
--    (1.08 secs, 546,144,264 bytes)
--    λ> maximaSuma3 (fromList 11 11 [1..])
--    1781
--    (0.55 secs, 217,712,280 bytes)
--    λ> maximaSuma4 (fromList 11 11 [1..])
--    1781
--    (0.01 secs, 643,832 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    maximaSuma1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
  it "e2" $
    maximaSuma1 (fromList 4 4 [1..]) `shouldBe` 73
  it "e3" $
    maximaSuma2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
  it "e4" $
    maximaSuma2 (fromList 4 4 [1..]) `shouldBe` 73
  it "e5" $
    maximaSuma3 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
  it "e6" $
    maximaSuma3 (fromList 4 4 [1..]) `shouldBe` 73
  it "e7" $
    maximaSuma4 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
  it "e8" $
    maximaSuma4 (fromList 4 4 [1..]) `shouldBe` 73

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--    e7
--    e8
--
--    Finished in 0.0034 seconds
--    8 examples, 0 failures
