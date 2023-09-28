-- Caminos_en_una_matriz.hs
-- Caminos en una matriz (con programación dinámica)
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-octubre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los caminos desde el extremo superior izquierdo (posición (1,1))
-- hasta el extremo inferior derecho (posición (3,4)) en la matriz
--    (  1  6 11  2 )
--    (  7 12  3  8 )
--    (  3  8  4  9 )
-- moviéndose en cada paso una casilla hacia la derecha o abajo, son los
-- siguientes:
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
--
-- Definir la función
--    caminos :: Matrix Int -> [[Int]]
-- tal que (caminos m) es la lista de los caminos en la matriz m desde
-- el extremo superior izquierdo hasta el extremo inferior derecho,
-- moviéndose en cada paso una casilla hacia abajo o hacia la
-- derecha. Por ejemplo,
--    λ> caminos (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    [[1,7, 3,8,4,9],
--     [1,7,12,8,4,9],
--     [1,7,12,3,4,9],
--     [1,7,12,3,8,9],
--     [1,6,12,8,4,9],
--     [1,6,12,3,4,9],
--     [1,6,12,3,8,9],
--     [1,6,11,3,4,9],
--     [1,6,11,3,8,9],
--     [1,6,11,2,8,9]]
--    λ> length (caminos (fromList 12 12 [1..]))
--    705432
-- ---------------------------------------------------------------------

module Caminos_en_una_matriz where

import Data.Matrix
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

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

-- 2ª solución (mediante programación dinámica)
-- ============================================

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

-- Nota: (caminos2 m) es la inversa de (caminos1 m).

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (caminos1 (fromList 11 11 [1..]))
--    184756
--    (3.64 secs, 667,727,568 bytes)
--    λ> length (caminos2 (fromList 11 11 [1..]))
--    184756
--    (0.82 secs, 129,181,072 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    caminos1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` r
  it "e2" $
    caminos2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` r
  where r = [[1,6,11,2,8,9],
             [1,6,11,3,8,9],
             [1,6,12,3,8,9],
             [1,7,12,3,8,9],
             [1,6,11,3,4,9],
             [1,6,12,3,4,9],
             [1,7,12,3,4,9],
             [1,6,12,8,4,9],
             [1,7,12,8,4,9],
             [1,7, 3,8,4,9]]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0010 seconds
--    2 examples, 0 failures
