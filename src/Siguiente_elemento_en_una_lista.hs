-- Siguiente_elemento_en_una_lista.hs
-- Siguiente elemento en una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 02-Febrero-2015 (actualizado 23-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    siguiente :: Eq a => a -> [a] -> Maybe a
-- tal que (siguiente x ys) es justo el elemento siguiente a la primera
-- ocurrencia de x en ys o Nothing si x no pertenece a ys. Por ejemplo,
--    siguiente 5 [3,5,2,5,7]                       ==  Just 2
--    siguiente 9 [3,5,2,5,7]                       ==  Nothing
--    siguiente 'd' "afdegdb"                       ==  Just 'e'
--    siguiente "todo" ["En","todo","la","medida"]  ==  Just "la"
--    siguiente "nada" ["En","todo","la","medida"]  ==  Nothing
--    let n=6*10^7 in siguiente (n-1) [1..n]        ==  Just 60000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Siguiente_elemento_en_una_lista where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

siguiente1 :: Eq a => a -> [a] -> Maybe a
siguiente1 x (y1:y2:ys)
  | x == y1   = Just y2
  | otherwise = siguiente1 x (y2:ys)
siguiente1 _ _ = Nothing

-- 2ª solución
-- ===========

siguiente2 :: Eq a => a -> [a] -> Maybe a
siguiente2 x ys
  | null zs   = Nothing
  | otherwise = Just (snd (head zs))
  where zs = [(u,v) | (u,v) <- zip ys (tail ys), u == x]

-- 3ª solución
-- ===========

siguiente3 :: Eq a => a -> [a] -> Maybe a
siguiente3 x ys = lookup x (zip ys (tail ys))

-- 4ª solución
-- ===========

siguiente4 :: Eq a => a -> [a] -> Maybe a
siguiente4 x = aux . drop 1 . dropWhile (/=x)
  where aux []    = Nothing
        aux (y:_) = Just y

-- 5ª solución
-- ===========

siguiente5 :: Eq a => a -> [a] -> Maybe a
siguiente5 x = listToMaybe . drop 1 . dropWhile (/=x)

-- 6ª solución
-- ===========

siguiente6 :: Eq a => a -> [a] -> Maybe a
siguiente6 x ys = case dropWhile (/= x) ys of
  (_:z:_) -> Just z
  _       -> Nothing

-- 7ª solución
-- ===========

siguiente7 :: Eq a => a -> [a] -> Maybe a
siguiente7 x ys = snd <$> find ((== x) . fst) (zip ys (tail ys))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> Maybe Int) -> Spec
specG siguiente = do
  it "e1" $
    siguiente 5 [3,5,2,5,7] `shouldBe` Just 2
  it "e2" $
    siguiente 9 [3,5,2,5,7] `shouldBe` Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG siguiente1
  describe "def. 2" $ specG siguiente2
  describe "def. 3" $ specG siguiente3
  describe "def. 4" $ specG siguiente4
  describe "def. 5" $ specG siguiente5
  describe "def. 6" $ specG siguiente6
  describe "def. 7" $ specG siguiente7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Int -> [Int] -> Bool
prop_equivalencia x ys =
  all (== siguiente1 x ys)
      [ siguiente2 x ys
      , siguiente3 x ys
      , siguiente4 x ys
      , siguiente5 x ys
      , siguiente6 x ys
      , siguiente7 x ys
      ]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    True

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> let n=8*10^6 in siguiente2 (n-1) [1..n]
--    Just 8000000
--    (2.54 secs, 1,664,607,312 bytes)
--    λ> let n=8*10^6 in siguiente3 (n-1) [1..n]
--    Just 8000000
--    (0.27 secs, 1,216,606,864 bytes)
--    λ> let n=8*10^6 in siguiente4 (n-1) [1..n]
--    Just 8000000
--    (0.65 secs, 576,607,360 bytes)
--    λ> let n=8*10^6 in siguiente5 (n-1) [1..n]
--    Just 8000000
--    (0.62 secs, 576,607,280 bytes)
--    λ> let n=8*10^6 in siguiente6 (n-1) [1..n]
--    Just 8000000
--    (1.33 secs, 576,607,000 bytes)
--    λ> let n=8*10^6 in siguiente7 (n-1) [1..n]
--    Just 8000000
--    (0.92 secs, 2,176,607,192 bytes)
--
--    λ> let n=3*10^7 in siguiente3 (n-1) [1..n]
--    Just 30000000
--    (1.00 secs, 4,560,607,808 bytes)
--    λ> let n=3*10^7 in siguiente4 (n-1) [1..n]
--    Just 30000000
--    (2.27 secs, 2,160,608,296 bytes)
--    λ> let n=3*10^7 in siguiente5 (n-1) [1..n]
--    Just 30000000
--    (2.32 secs, 2,160,608,160 bytes)
