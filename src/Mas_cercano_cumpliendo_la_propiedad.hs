-- Mas_cercano_cumpliendo_la_propiedad.hs
-- Elemento más cercano que cumple una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-Julio-2014 (actualizado 14-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    cercano :: (a -> Bool) -> Int -> [a] -> Maybe a
-- tal que (cercano p n xs) es el elemento de xs cuya posición es la más
-- cercana a la posición n que verifica la propiedad p. La búsqueda
-- comienza en n y los elementos se analizan en el siguiente orden: n,
-- n+1, n-1, n+2, n-2,... Por ejemplo,
--    cercano (`elem` "aeiou") 6 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 1 "Sevilla"     ==  Just 'e'
--    cercano (`elem` "aeiou") 2 "Sevilla"     ==  Just 'i'
--    cercano (`elem` "aeiou") 5 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 9 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") (-3) "Sevilla"  ==  Just 'e'
--    cercano (>100) 4 [200,1,150,2,4]         ==  Just 150
--    cercano even 5 [1,3..99]                 ==  Nothing
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Mas_cercano_cumpliendo_la_propiedad where

import Data.List (find)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

cercano1 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano1 p n xs | null ys   = Nothing
                | otherwise = Just (head ys)
    where ys = filter p (ordenaPorCercanos xs n)

-- (ordenaPorCercanos xs n) es la lista de los elementos de xs que
-- ocupan las posiciones n, n+1, n-1, n+2, n-2... Por ejemplo,
--    ordenaPorCercanos [0..9] 4     ==  [4,5,3,6,2,7,1,8,0,9]
--    ordenaPorCercanos [0..9] 7     ==  [7,8,6,9,5,4,3,2,1,0]
--    ordenaPorCercanos [0..9] 2     ==  [2,3,1,4,0,5,6,7,8,9]
--    ordenaPorCercanos [0..9] (-3)  ==  [0,1,2,3,4,5,6,7,8,9]
--    ordenaPorCercanos [0..9] 20    ==  [9,8,7,6,5,4,3,2,1,0]
ordenaPorCercanos :: [a] -> Int -> [a]
ordenaPorCercanos xs n
    | n < 0          = xs
    | n >= length xs = reverse xs
    | otherwise      = z : intercala zs (reverse ys)
    where (ys,(z:zs)) = splitAt n xs

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- las lista xs e ys. Por ejemplo,
--    intercala [1..4] [5..10]   ==  [1,5,2,6,3,7,4,8,9,10]
--    intercala [5..10] [1..4]   ==  [5,1,6,2,7,3,8,4,9,10]
intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 2ª solución
-- ===========

cercano2 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano2 p n xs = find p (ordenaPorCercanos xs n)

-- 3ª solución
-- ===========

cercano3 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano3 p n xs = buscaCercano 0
  where
    len = length xs

    buscaCercano k
      | k == 0 =
          if enRango n && p (xs !! n)
          then Just (xs !! n)
          else buscaCercano 1
      | otherwise =
          case (elemCumple (n + k), elemCumple (n - k)) of
            (Just x, _) -> Just x
            (_, Just x) -> Just x
            _ -> if n + k >= len && n - k < 0
                 then Nothing
                 else buscaCercano (k + 1)

    enRango i = i >= 0 && i < len

    elemCumple i
      | enRango i && p (xs !! i) = Just (xs !! i)
      | otherwise = Nothing

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Char -> Bool) -> Int -> String -> Maybe Char) -> Spec
specG cercano = do
  it "e1" $
    cercano (`elem` "aeiou") 6 "Sevilla"     `shouldBe`  Just 'a'
  it "e2" $
    cercano (`elem` "aeiou") 1 "Sevilla"     `shouldBe`  Just 'e'
  it "e3" $
    cercano (`elem` "aeiou") 2 "Sevilla"     `shouldBe`  Just 'i'
  it "e4" $
    cercano (`elem` "aeiou") 5 "Sevilla"     `shouldBe`  Just 'a'
  it "e5" $
    cercano (`elem` "aeiou") 9 "Sevilla"     `shouldBe`  Just 'a'
  it "e6" $
    cercano (`elem` "aeiou") (-3) "Sevilla"  `shouldBe`  Just 'e'
  it "e7" $
    cercano (`elem` "xyz") (-3) "Sevilla"    `shouldBe`  Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG cercano1
  describe "def. 2" $ specG cercano2
  describe "def. 3" $ specG cercano3

-- La verificación es
--    λ> verifica
--    21 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_cercano :: String -> Int -> String -> Bool
prop_cercano xs n cs =
  all (== cercano1 p n xs)
      [cercano2 p n xs,
       cercano3 p n xs]
  where
    p = (`elem` cs)

-- La comprobación es
--    λ> quickCheck prop_cercano
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> cercano1 (< 100) 10000 [0..20000]
--    Just 99
--    (0.02 secs, 5,700,384 bytes)
--    λ> cercano2 (< 100) 10000 [0..20000]
--    Just 99
--    (0.01 secs, 7,442,952 bytes)
--    λ> cercano3 (< 100) 10000 [0..20000]
--    Just 99
--    (0.89 secs, 17,487,240 bytes)
--    λ> cercano1 (< 0) 10000 [0..20000]
--    Nothing
--    (0.02 secs, 5,721,232 bytes)
--    λ> cercano2 (< 0) 10000 [0..20000]
--    Nothing
--    (0.01 secs, 7,481,312 bytes)
--    λ> cercano3 (< 0) 10000 [0..20000]
--    Nothing
--    (0.90 secs, 17,642,904 bytes)
