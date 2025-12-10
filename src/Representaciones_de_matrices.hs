-- Representaciones_de_matrices.hs
-- Representaciones de matrices.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-Diciembre-2014 (actualizado 10-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices se pueden representar de distintas formas. Por ejemplo,
-- la matriz
--    |7 5 6|
--    |1 9 4|
-- se puede representar como la terna
--    ([7,5,6,1,9,4],2,3)
-- (donde la primera componente es la lista de los elementos de la
-- matriz, la segunda es su número de filas y la tercera es su número de
-- columnas) y también se puede representar como una lista de listas
--    [[[7,5,6],[1,9,4]]
-- (donde cada elemento es una de las filas de la matriz).
--
-- Definir las funciones
--    ternaAlistas :: ([a],Int,Int) -> [[a]]
--    listasAterna :: [[a]] -> ([a],Int,Int)
-- tales que ternaAlistas pase de la primera representación a la
-- segunda y listasAterna pase de la segunda a la primera. Por ejemplo,
--    ternaAlistas ([7,5,6,1,9,4],2,3)  ==  [[7,5,6],[1,9,4]]
--    listasAterna [[7,5,6],[1,9,4]]    ==  ([7,5,6,1,9,4],2,3)
--    ternaAlistas ([7,5,6,1,9,4],3,2)  ==  [[7,5],[6,1],[9,4]]
--    listasAterna [[7,5],[6,1],[9,4]]  ==  ([7,5,6,1,9,4],3,2)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Representaciones_de_matrices where

import Data.List.Split (chunksOf)
import Control.Applicative (liftA3)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª definición de ternaAlistas
-- =============================

ternaAlistas1 :: ([a],Int,Int) -> [[a]]
ternaAlistas1 (xs,_,m) = segmentos m xs

-- (segmentos m xs) es la lista de los segmentos de xs formados por m
-- elementos. Por ejemplo,
--    segmentos 3 [7,5,6,1,9,4] == [[7,5,6],[1,9,4]]
--    segmentos 2 [7,5,6,1,9,4] == [[7,5],[6,1],[9,4]]
segmentos :: Int -> [a] -> [[a]]
segmentos _ [] = []
segmentos m xs = take m xs : segmentos m (drop m xs)

-- 2ª definición de ternaAlistas
-- =============================

ternaAlistas2 :: ([a],Int,Int) -> [[a]]
ternaAlistas2 (xs,_,m) = chunksOf m xs

-- 1ª definición de listasAterna
-- =============================

listasAterna1 :: [[a]] -> ([a],Int,Int)
listasAterna1 xss = (concat xss, length xss, length (head xss))

-- 2ª definición de listasAterna
-- =============================

listasAterna2 :: [[a]] -> ([a], Int, Int)
listasAterna2 = liftA3 (,,) concat length (length . head)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: (([Int], Int, Int) -> [[Int]]) -> Spec
specG1 ternaAlistas = do
  it "e1" $
    ternaAlistas ([7,5,6,1,9,4],2,3) `shouldBe` [[7,5,6],[1,9,4]]
  it "e2" $
    ternaAlistas ([7,5,6,1,9,4],3,2) `shouldBe` [[7,5],[6,1],[9,4]]

specG2 :: ([[Int]] -> ([Int],Int,Int)) -> Spec
specG2 listasAterna = do
  it "e1" $
    listasAterna [[7,5,6],[1,9,4]]   `shouldBe` ([7,5,6,1,9,4],2,3)
  it "e2" $
    listasAterna [[7,5],[6,1],[9,4]] `shouldBe` ([7,5,6,1,9,4],3,2)

spec :: Spec
spec = do
  describe "def. 1" $ specG1 ternaAlistas1
  describe "def. 2" $ specG1 ternaAlistas2
  describe "def. 1" $ specG2 listasAterna1
  describe "def. 2" $ specG2 listasAterna2

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de ternas. Por ejemplo,
--    λ> generate genTerna
--    ([14,-7,27,-25,30,0,18,7],2,4)
--    λ> generate genTerna
--    ([19,-11,-3,-18,23],5,1)
genTerna :: Gen ([Int], Int, Int)
genTerna = do
    n <- choose (1, 5)
    m <- choose (1, 5)
    xs <- vector (n * m)
    return (xs, n, m)

-- La propiedad es
prop_equiv_ternaAlistas :: Property
prop_equiv_ternaAlistas =
  forAll genTerna $ \t -> ternaAlistas1 t == ternaAlistas2 t

-- La comprobación es
--    λ> quickCheck prop_equiv_ternaAlistas
--    +++ OK, passed 100 tests.

-- genListas es un generador de lista de listas todas de la misma
-- longitud. Por ejemplo,
--    λ> generate genListas
--    [[0,-3,6],[14,20,-19,-21,25]]
--    λ> generate genListas
--    [[21,3,-19],[-14],[-4,-19,20,23],[-13,-2,-23,-10,28]]
genListas :: Gen [[Int]]
genListas = do
  n <- choose (1, 5)
  vectorOf n $ do
    m <- choose (1, 5)
    vectorOf m arbitrary

-- La propiedad es
prop_equiv_listasAterna :: Property
prop_equiv_listasAterna =
  forAll genListas $ \xss -> listasAterna1 xss == listasAterna2 xss

-- La comprobación es
--    λ> quickCheck prop_equiv_listasAterna
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (ternaAlistas1 ([1..10^4*10^4],10^4,10^4))
--    10000
--    (1.63 secs, 7,203,081,632 bytes)
--    λ> length (ternaAlistas2 ([1..10^4*10^4],10^4,10^4))
--    10000
--    (1.59 secs, 7,202,361,648 bytes)
--    λ> ejemplo = replicate (10^4) (replicate (10^4) 0)
--    λ> listasAterna1 ejemplo == listasAterna1 ejemplo
--    True
--    (1.99 secs, 11,203,482,192 bytes)
--    λ> listasAterna2 ejemplo == listasAterna2 ejemplo
--    True
--    (2.04 secs, 11,203,482,248 bytes)
