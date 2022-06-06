-- Representacion_matricial_de_relaciones_binarias.hs
-- Representación matricial de relaciones binarias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dada una relación r sobre un conjunto de números enteros, la matriz
-- asociada a r es una matriz booleana p (cuyos elementos son True o
-- False), tal que p(i,j) = True si y sólo si i está relacionado con j
-- mediante la relación r. 
-- 
-- Las relaciones binarias homogéneas y las matrices booleanas se pueden
-- representar por 
--    type Relacion = ([Int],[(Int,Int)])
--    type Matriz = Array (Int,Int) Bool
-- 
-- Definir la función
--    matrizRB:: Relacion -> Matriz
-- tal que (matrizRB r) es la matriz booleana asociada a r. Por ejemplo, 
--    λ> matrizRB ([1..3],[(1,1), (1,3), (3,1), (3,3)])
--    array ((1,1),(3,3)) [((1,1),True) ,((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True) ,((3,2),False),((3,3),True)]
--    λ> matrizRB ([1..3],[(1,3), (3,1)])
--    array ((1,1),(3,3)) [((1,1),False),((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True) ,((3,2),False),((3,3),False)]
--    λ> let n = 10^4 in matrizRB3 ([1..n],[(1,n),(n,1)]) ! (n,n)
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Representacion_matricial_de_relaciones_binarias where

import Data.Array      (Array, accumArray, array, listArray)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, sublistOf, suchThat,
                        quickCheck)

type Relacion = ([Int],[(Int,Int)])
type Matriz   = Array (Int,Int) Bool

-- 1ª solución
-- ===========

matrizRB1 :: Relacion -> Matriz 
matrizRB1 r = 
    array ((1,1),(n,n)) 
          [((a,b), (a,b) `elem` grafo r) | a <- [1..n], b <- [1..n]]
    where n = maximum (universo r)
          universo (us,_) = us
          grafo (_,ps)    = ps

-- 2ª solución
-- ===========

matrizRB2 :: Relacion -> Matriz
matrizRB2 r = 
    listArray ((1,1),(n,n)) 
              [(a,b) `elem` snd r | a <- [1..n], b <- [1..n]]
    where n = maximum (fst r)

-- 3ª solución
-- ===========

matrizRB3 :: Relacion -> Matriz
matrizRB3 r = 
    accumArray (||) False ((1,1),(n,n)) (zip (snd r) (repeat True))
    where n = maximum (fst r)

-- Comprobación de equivalencia
-- ============================

-- Tipo de relaciones binarias
newtype RB = RB Relacion
  deriving Show

-- relacionArbitraria genera una relación arbitraria. Por ejemplo, 
--    λ> generate relacionArbitraria
--    RB ([1,2,3,4,5],[(1,4),(1,5),(2,3),(2,4),(4,2),(4,3),(4,4),(5,1),(5,2),(5,3),(5,4)])
relacionArbitraria :: Gen RB
relacionArbitraria = do
  n <- arbitrary `suchThat` (> 1)
  xs <- sublistOf [(x,y) | x <- [1..n], y <- [1..n]]
  return (RB ([1..n], xs))

-- RB es una subclase de Arbitrary
instance Arbitrary RB where
  arbitrary = relacionArbitraria

-- La propiedad es
prop_matrizRB :: RB -> Bool
prop_matrizRB (RB r) =
  all (== matrizRB1 r)
      [matrizRB2 r,
       matrizRB3 r]

-- La comprobación es
--    λ> quickCheck prop_matrixzB
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> let n = 2000 in matrizRB1 ([1..n],[(1,n),(n,1)]) ! (n,n)
--    False
--    (2.02 secs, 1,505,248,912 bytes)
--    λ> let n = 2000 in matrizRB2 ([1..n],[(1,n),(n,1)]) ! (n,n)
--    False
--    (1.92 secs, 833,232,360 bytes)
--    λ> let n = 2000 in matrizRB3 ([1..n],[(1,n),(n,1)]) ! (n,n)
--    False
--    (0.05 secs, 32,848,696 bytes)
