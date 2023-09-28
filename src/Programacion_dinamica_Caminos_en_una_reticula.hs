-- Programacion_dinamica_Caminos_en_una_reticula.hs
-- Caminos en una retícula (con programación dinámica)
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-octubre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se considera una retícula con sus posiciones numeradas, desde el
-- vértice superior izquierdo, hacia la derecha y hacia abajo. Por
-- ejemplo, la retícula de dimensión 3x4 se numera como sigue:
--    |-------+-------+-------+-------|
--    | (1,1) | (1,2) | (1,3) | (1,4) |
--    | (2,1) | (2,2) | (2,3) | (2,4) |
--    | (3,1) | (3,2) | (3,3) | (3,4) |
--    |-------+-------+-------+-------|
--
-- Definir la función
--    caminos :: (Int,Int) -> [[(Int,Int)]]
-- tal que (caminos (m,n)) es la lista de los caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    λ> caminos (2,3)
--    [[(1,1),(1,2),(1,3),(2,3)],
--     [(1,1),(1,2),(2,2),(2,3)],
--     [(1,1),(2,1),(2,2),(2,3)]]
--    λ> mapM_ print (caminos (3,4))
--    [(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]
-- ---------------------------------------------------------------------

module Programacion_dinamica_Caminos_en_una_reticula where

import Data.Array (Array, (!), array)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª solución (por recursión)
-- ===========================

caminos1 :: (Int,Int) -> [[(Int,Int)]]
caminos1 p = map reverse (caminos1Aux p)
  where
    caminos1Aux (1,y) = [[(1,z) | z <- [y,y-1..1]]]
    caminos1Aux (x,1) = [[(z,1) | z <- [x,x-1..1]]]
    caminos1Aux (x,y) = [(x,y) : cs | cs <- caminos1Aux (x-1,y) ++
                                            caminos1Aux (x,y-1)]

-- 2ª solución (con programación dinámica)
-- =======================================

caminos2 :: (Int,Int) -> [[(Int,Int)]]
caminos2 p = map reverse (matrizCaminos p ! p)

matrizCaminos :: (Int,Int) -> Array (Int,Int) [[(Int,Int)]]
matrizCaminos (m,n) = q
  where
    q = array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    f 1 y = [[(1,z) | z <- [y,y-1..1]]]
    f x 1 = [[(z,1) | z <- [x,x-1..1]]]
    f x y = [(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> maximum (head (caminos1 (2000,2000)))
--    (2000,2000)
--    (0.01 secs, 3,459,576 bytes)
--    λ> maximum (head (caminos2 (2000,2000)))
--    (2000,2000)
--    (2.79 secs, 1,507,636,688 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    caminos1 (2,3) `shouldBe`
    [[(1,1),(1,2),(1,3),(2,3)],
     [(1,1),(1,2),(2,2),(2,3)],
     [(1,1),(2,1),(2,2),(2,3)]]
  it "e2" $
    caminos2 (2,3) `shouldBe`
    [[(1,1),(1,2),(1,3),(2,3)],
     [(1,1),(1,2),(2,2),(2,3)],
     [(1,1),(2,1),(2,2),(2,3)]]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0010 seconds
--    2 examples, 0 failures
