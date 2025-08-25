-- Buscaminas.hs
-- Buscaminas.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 19-Junio-2014 (Revisión del 22-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El buscaminas es un juego cuyo objetivo es despejar un campo de minas
-- sin detonar ninguna.
--
-- El campo de minas se representa mediante un cuadrado con NxN
-- casillas. Algunas casillas tienen un número, este número indica las
-- minas que hay en todas las casillas vecinas. Cada casilla tiene como
-- máximo 8 vecinas. Por ejemplo, el campo 4x4 de la izquierda
-- contiene dos minas, cada una representada por el número 9, y a la
-- derecha se muestra el campo obtenido anotando las minas vecinas de
-- cada casilla
--    9 0 0 0       9 1 0 0
--    0 0 0 0       2 2 1 0
--    0 9 0 0       1 9 1 0
--    0 0 0 0       1 1 1 0
-- de la misma forma, la anotación del siguiente a la izquierda es el de
-- la derecha
--    9 9 0 0 0     9 9 1 0 0
--    0 0 0 0 0     3 3 2 0 0
--    0 9 0 0 0     1 9 1 0 0
--
-- Utilizando la librería Data.Matrix, los campos de minas se
-- representan mediante matrices:
--    type Campo = Matrix Int
-- Por ejemplo, los anteriores campos de la izquierda se definen por
--    ejCampo1, ejCampo2 :: Campo
--    ejCampo1 = fromLists [[9,0,0,0],
--                          [0,0,0,0],
--                          [0,9,0,0],
--                          [0,0,0,0]]
--    ejCampo2 = fromLists [[9,9,0,0,0],
--                          [0,0,0,0,0],
--                          [0,9,0,0,0]]
--
-- Definir la función
--    buscaminas :: Campo -> Campo
-- tal que (buscaminas c) es el campo obtenido anotando las minas
-- vecinas de cada casilla. Por ejemplo,
--    ghci> buscaminas ejCampo1
--    ( 9 1 0 0 )
--    ( 2 2 1 0 )
--    ( 1 9 1 0 )
--    ( 1 1 1 0 )
--
--    ghci> buscaminas ejCampo2
--    ( 9 9 1 0 0 )
--    ( 3 3 2 0 0 )
--    ( 1 9 1 0 0 )
--
-- Notas.
-- 1. El manual de la librería Data.Matrix se encuentra en
--    https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html
-- 2. Las funciones de dicha librería útiles para este ejercicio son
--    fromLists, matrix, nrows y ncols.
-- ---------------------------------------------------------------------

module Buscaminas where

import Data.Matrix
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

type Campo   = Matrix Int
type Casilla = (Int,Int)

ejCampo1, ejCampo2 :: Campo
ejCampo1 = fromLists [[9,0,0,0],
                      [0,0,0,0],
                      [0,9,0,0],
                      [0,0,0,0]]
ejCampo2 = fromLists [[9,9,0,0,0],
                      [0,0,0,0,0],
                      [0,9,0,0,0]]

-- 1ª solución
-- ===========

buscaminas1 :: Campo -> Campo
buscaminas1 c = matrix m n (\(i,j) -> minas c (i,j))
  where m = nrows c
        n = ncols c

-- (minas c (i,j)) es el número de minas en las casillas vecinas de la
-- (i,j) en el campo de mina c y es 9 si en (i,j) hay una mina. Por
-- ejemplo,
--    minas ejCampo (1,1)  ==  9
--    minas ejCampo (1,2)  ==  1
--    minas ejCampo (1,3)  ==  0
--    minas ejCampo (2,1)  ==  2
minas :: Campo -> Casilla -> Int
minas c (i,j)
  | c!(i,j) == 9 = 9
  | otherwise    = length (filter (==9) [c!(x,y) | (x,y) <- vecinas m n (i,j)])
  where m = nrows c
        n = ncols c

-- (vecinas m n (i,j)) es la lista de las casillas vecinas de la (i,j) en
-- un campo de dimensiones mxn. Por ejemplo,
--    vecinas 4 (1,1)  ==  [(1,2),(2,1),(2,2)]
--    vecinas 4 (1,2)  ==  [(1,1),(1,3),(2,1),(2,2),(2,3)]
--    vecinas 4 (2,3)  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
vecinas :: Int -> Int -> Casilla -> [Casilla]
vecinas m n (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                             b <- [max 1 (j-1)..min n (j+1)],
                             (a,b) /= (i,j)]

-- 2ª solución
-- ===========

buscaminas2 :: Campo -> Campo
buscaminas2 c = matrix m n (\(i,j) -> minas' (i,j))
  where m = nrows c
        n = ncols c
        minas' :: Casilla -> Int
        minas' (i,j)
          | c!(i,j) == 9 = 9
          | otherwise    =
              length (filter (==9) [c!(x,y) | (x,y) <- vecinas' (i,j)])
        vecinas' :: Casilla -> [Casilla]
        vecinas' (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                  b <- [max 1 (j-1)..min n (j+1)],
                                  (a,b) /= (i,j)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Campo -> Campo)  -> Spec
specG buscaminas = do
  it "e1" $
    toLists (buscaminas ejCampo1) `shouldBe`
    [[9,1,0,0],[2,2,1,0],[1,9,1,0],[1,1,1,0]]
  it "e2" $
    toLists (buscaminas ejCampo2) `shouldBe`
    [[9,9,1,0,0],[3,3,2,0,0],[1,9,1,0,0]]

spec :: Spec
spec = do
  describe "def. 1" $ specG buscaminas1
  describe "def. 2" $ specG buscaminas2

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures
