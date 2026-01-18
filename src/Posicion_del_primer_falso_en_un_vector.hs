-- Posicion_del_primer_falso_en_un_vector.hs
-- Posición del primer falso en un vector.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 29-Enero-2015 (actualizado 18-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    posicion :: Array Int Bool -> Maybe Int
-- tal que (posicion v) es la menor posición del vector de booleanos v
-- cuyo valor es falso y es Nothing si todos los valores son
-- verdaderos. Por ejemplo,
--    posicion (listArray (0,4) [True,True,False,True,False]) == Just 2
--    posicion (listArray (0,4) [i <= 2 | i <- [0..4]])       == Just 3
--    posicion (listArray (0,4) [i <= 7 | i <- [0..4]])       == Nothing
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Posicion_del_primer_falso_en_un_vector where

import Data.Array (Array, (!), assocs, bounds, elems, indices, listArray)
import Data.List (find, findIndex)
import Data.Maybe (listToMaybe)
import qualified Data.Vector as V (findIndex, fromList)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

posicion1 :: Array Int Bool -> Maybe Int
posicion1 v | p > n     = Nothing
            | otherwise = Just p
  where p = (length . takeWhile id . elems) v
        (_,n) = bounds v

-- 2ª solución
-- ===========

posicion2 :: Array Int Bool -> Maybe Int
posicion2 v | null xs   = Nothing
            | otherwise = Just (head xs)
  where xs = posiciones2 v

-- (posiciones v) es la de las posiciones donde el elemento de v es
-- falso. Por ejemplo,
--    λ> posiciones (listArray (0,4) [True,True,False,True,False])
--    [2,4]
--    λ> posiciones (listArray (0,4) [i <= 2 | i <- [0..4]])
--    [3,4]
--    λ> posiciones (listArray (0,4) [i <= 7 | i <- [0..4]])
--    []
posiciones2 :: Array Int Bool -> [Int]
posiciones2 v = [i | i <- indices v, not (v!i)]

-- 2ª solución
-- ===========

posicion3 :: Array Int Bool -> Maybe Int
posicion3 v =
  case posiciones3 v of
    []    -> Nothing
    (x:_) -> Just x

posiciones3 :: Array Int Bool -> [Int]
posiciones3 v = [i | (i, x) <- assocs v, not x]

-- 4ª solución
-- ===========

posicion4 :: Array Int Bool -> Maybe Int
posicion4 v = listToMaybe [i | (i, x) <- assocs v, not x]

-- 5ª solución
-- ===========

posicion5 :: Array Int Bool -> Maybe Int
posicion5 v = find (\i -> not (v ! i)) (indices v)

-- 6ª solución
-- ===========

posicion6 :: Array Int Bool -> Maybe Int
posicion6 v =
  foldr (\i n -> if not (v ! i) then Just i else n) Nothing (indices v)

-- 7ª solución
-- ===========

posicion7 :: Array Int Bool -> Maybe Int
posicion7 v = buscar a
  where
    (a, b) = bounds v
    buscar i
      | i > b     = Nothing
      | not (v!i) = Just i
      | otherwise = buscar (i + 1)

-- 8ª solución
-- ===========

posicion8 :: Array Int Bool -> Maybe Int
posicion8 v =
  case findIndex not (elems v) of
    Nothing -> Nothing
    Just i  -> Just (i + fst (bounds v))

-- 9ª solución
-- ===========

posicion9 :: Array Int Bool -> Maybe Int
posicion9 v = V.findIndex not v'
  where v' = V.fromList (elems v)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Array Int Bool -> Maybe Int) -> Spec
specG posicion = do
  it "e1" $
    posicion (listArray (0,4) [True,True,False,True,False]) `shouldBe` Just 2
  it "e2" $
    posicion (listArray (0,4) [i <= 2 | i <- [0..4]])       `shouldBe` Just 3
  it "e3" $
    posicion (listArray (0,4) [i <= 7 | i <- [0..4]])       `shouldBe` Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG posicion1
  describe "def. 2" $ specG posicion2
  describe "def. 3" $ specG posicion3
  describe "def. 4" $ specG posicion4
  describe "def. 5" $ specG posicion5
  describe "def. 6" $ specG posicion6
  describe "def. 7" $ specG posicion7
  describe "def. 8" $ specG posicion8
  describe "def. 9" $ specG posicion9

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- Generador de vectores cuyos elementos son booleanos. Por ejemplo,
--    λ> generate genVector
--    array (0,3) [(0,False),(1,True),(2,True),(3,False)]
genVector :: Gen (Array Int Bool)
genVector = do
    n <- choose (0, 100)
    vals <- vectorOf (n + 1) arbitrary
    return (listArray (0, n) vals)

-- La propiedad es
prop_equivalencia :: Property
prop_equivalencia =
  forAll genVector $ \v ->
  all (== posicion1 v)
      [posicion2 v,
       posicion3 v,
       posicion4 v,
       posicion5 v,
       posicion6 v,
       posicion7 v,
       posicion8 v,
       posicion9 v]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> posicion1 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.24 secs, 2,528,623,872 bytes)
--    λ> posicion2 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.87 secs, 2,704,625,544 bytes)
--    λ> posicion3 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.93 secs, 2,704,623,824 bytes)
--    λ> posicion4 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.70 secs, 2,704,623,784 bytes)
--    λ> posicion5 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.71 secs, 2,784,623,808 bytes)
--    λ> posicion6 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (3.04 secs, 2,768,623,744 bytes)
--    λ> posicion7 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (3.24 secs, 2,896,623,792 bytes)
--    λ> posicion8 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.26 secs, 2,416,623,784 bytes)
--    λ> posicion9 (listArray (0,2*10^6) [i < 2*10^6 | i <- [0..2*10^6]])
--    Just 2000000
--    (2.25 secs, 2,450,211,376 bytes)
