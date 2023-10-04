-- Camino_de_maxima_suma_en_una_matriz.hs
-- Camino de máxima suma en una matriz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-octubre-2023
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
--    caminoMaxSuma :: Matrix Int -> [Int]
-- tal que (caminoMaxSuma m) es un camino de máxima suma en la matriz m
-- desde el extremo superior izquierdo hasta el extremo inferior derecho,
-- moviéndose en cada paso una casilla hacia abajo o hacia la
-- derecha. Por ejemplo,
--    λ> caminoMaxSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    [1,7,12,8,4,9]
--    λ> sum (caminoMaxSuma (fromList 500 500 [1..]))
--    187001249
-- ---------------------------------------------------------------------

module Camino_de_maxima_suma_en_una_matriz where

import Data.Matrix (Matrix, (!), fromLists, matrix, nrows, ncols)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición de caminoMaxSuma (con caminos1)
-- =============================================

caminoMaxSuma1 :: Matrix Int -> [Int]
caminoMaxSuma1 m =
  head [c | c <- cs, sum c == k]
  where cs = caminos1 m
        k  = maximum (map sum cs)

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

-- 2ª definición de caminoMaxSuma (con caminos2)
-- =============================================

caminoMaxSuma2 :: Matrix Int -> [Int]
caminoMaxSuma2 m =
  head [c | c <- cs, sum c == k]
  where cs = caminos2 m
        k  = maximum (map sum cs)

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

-- 3ª definición de caminoMaxSuma (con programación dinámica)
-- ==========================================================

caminoMaxSuma3 :: Matrix Int -> [Int]
caminoMaxSuma3 m = reverse (snd (q ! (nf,nc)))
  where nf = nrows m
        nc = ncols m
        q  = caminoMaxSumaAux m

caminoMaxSumaAux :: Matrix Int -> Matrix (Int,[Int])
caminoMaxSumaAux m = q
  where
    nf = nrows m
    nc = ncols m
    q  = matrix nf nc f
      where
        f (1,1) = (m!(1,1),[m!(1,1)])
        f (1,j) = (k + m!(1,j), m!(1,j):xs)
          where (k,xs) = q!(1,j-1)
        f (i,1) = (k + m!(i,1), m!(i,1):xs)
          where (k,xs) = q!(i-1,1)
        f (i,j) | k1 > k2   = (k1 + m!(i,j), m!(i,j):xs)
                | otherwise = (k2 + m!(i,j), m!(i,j):ys)
          where (k1,xs) = q!(i,j-1)
                (k2,ys) = q!(i-1,j)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (caminoMaxSuma1 (fromList 11 11 [1..]))
--    21
--    (3.92 secs, 1,778,557,904 bytes)
--    λ> length (caminoMaxSuma2 (fromList 11 11 [1..]))
--    21
--    (1.16 secs, 798,889,488 bytes)
--    λ> length (caminoMaxSuma3 (fromList 11 11 [1..]))
--    21
--    (0.00 secs, 680,256 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    caminoMaxSuma1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
    `shouldBe` [1,7,12,8,4,9]
  it "e2" $
    caminoMaxSuma2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
    `shouldBe` [1,7,12,8,4,9]
  it "e3" $
    caminoMaxSuma3 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
    `shouldBe` [1,7,12,8,4,9]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.0007 seconds
--    3 examples, 0 failures
