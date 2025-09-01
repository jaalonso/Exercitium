-- Buscaminas.hs
-- Buscaminas.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 19-Junio-2014 (actualizado 1-Septiembre-2025)
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
--    λ> buscaminas1 ejCampo1
--    ┌         ┐
--    │ 9 1 0 0 │
--    │ 2 2 1 0 │
--    │ 1 9 1 0 │
--    │ 1 1 1 0 │
--    └         ┘
--    λ> buscaminas1 ejCampo2
--    ┌           ┐
--    │ 9 9 1 0 0 │
--    │ 3 3 2 0 0 │
--    │ 1 9 1 0 0 │
--    └           ┘
--
-- Notas.
-- 1. El manual de la librería Data.Matrix se encuentra en
--    https://hackage.haskell.org/package/matrix-0.3.6.1/docs/Data-Matrix.html
-- 2. Las funciones de dicha librería útiles para este ejercicio son
--    fromLists, matrix, nrows y ncols.
-- ---------------------------------------------------------------------

module Buscaminas where

import Data.List (foldl')
import Data.Matrix (Matrix, (!), fromLists, matrix, mapPos, nrows, ncols, toLists)
import Control.Monad (replicateM)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

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

-- 3ª solución
-- ===========

buscaminas3 :: Campo -> Campo
buscaminas3 c = matrix m n (\(i,j) -> minas2 c (i,j))
  where m = nrows c
        n = ncols c

minas2 :: Campo -> Casilla -> Int
minas2 c (i,j)
  | c!(i,j) == 9 = 9
  | otherwise    = foldl' (\acc v -> if v == 9 then acc+1 else acc)
                          0
                          [c!(x,y) | (x,y) <- vecinas m n (i,j)]
  where m = nrows c
        n = ncols c

-- 4ª solución
-- ===========

buscaminas4 :: Campo -> Campo
buscaminas4 campo = mapPos f campo
  where
    f (i,j) val
        | val == 9 = 9
        | otherwise = contarAlrededor (i,j)
    contarAlrededor (i,j) = length
        [ () | di <- [-1..1], dj <- [-1..1], (di,dj) /= (0,0)
             , let ni = i+di, let nj = j+dj
             , inRange ni nj
             , campo ! (ni,nj) == 9 ]
    inRange i j = i >= 1 && i <= nrows campo
               && j >= 1 && j <= ncols campo

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
  describe "def. 3" $ specG buscaminas3
  describe "def. 4" $ specG buscaminas4

-- La verificación es
--    λ> verifica
--    4 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

newtype Campo2 = C Campo

instance Show Campo2 where
  show (C p) = show p

-- Generador aleatorio de una casilla (0 = vacío, 9 = mina). Por
-- ejemplo,
--    λ> generate genCasilla
--    9
--    λ> generate genCasilla
--    0
genCasilla :: Gen Int
genCasilla = elements [0,9]

-- Generador de campos. Por ejemplo,
--    λ> generate (genCampo 5)
--    ┌         ┐
--    │ 0 9 9 0 │
--    │ 9 0 0 9 │
--    │ 0 9 0 0 │
--    └         ┘
genCampo :: Int -> Gen Campo2
genCampo a = do
  let cota = max 1 a
  m <- choose (1,cota)
  n <- choose (1,cota)
  rows <- replicateM m (replicateM n genCasilla)
  return (C (fromLists rows))

instance Arbitrary Campo2 where
  arbitrary = sized genCampo

-- La propiedad es
prop_buscaminas :: Campo2 -> Bool
prop_buscaminas (C p) =
  all (== buscaminas1 p)
      [buscaminas2 p,
       buscaminas3 p,
       buscaminas4 p]

-- La comprobación es
--    λ> quickCheck prop_buscaminas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> C p <- generate (genCampo 20000)
--    λ> length (buscaminas1 p)
--    9414947
--    (3.41 secs, 3,164,606,704 bytes)
--    λ> length (buscaminas2 p)
--    9414947
--    (1.21 secs, 678,499,992 bytes)
--    λ> length (buscaminas3 p)
--    9414947
--    (0.50 secs, 678,499,952 bytes)
--    λ> length (buscaminas4 p)
--    9414947
--    (0.86 secs, 678,499,992 bytes)
