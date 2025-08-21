-- Iguales_al_siguiente.hs
-- Iguales al siguiente.
-- José A. Alonso Jiménez https://jaalonso.github.io
-- Sevilla, 21 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    igualesAlSiguiente :: Eq a => [a] -> [a]
-- tal que (igualesAlSiguiente xs) es la lista de los elementos de xs
-- que son iguales a su siguiente. Por ejemplo,
--    igualesAlSiguiente [1,2,2,2,3,3,4]  ==  [2,2,3]
--    igualesAlSiguiente [1..10]          ==  []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Iguales_al_siguiente where

import Data.List (group)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª definición (por comprensión):
igualesAlSiguiente1 :: Eq a => [a] -> [a]
igualesAlSiguiente1 xs =
    [x | (x,y) <- zip xs (tail xs), x == y]

-- 2ª definición (por recursión):
igualesAlSiguiente2 :: Eq a => [a] -> [a]
igualesAlSiguiente2 (x:y:zs) | x == y    = x : igualesAlSiguiente2 (y:zs)
                             | otherwise = igualesAlSiguiente2 (y:zs)
igualesAlSiguiente2 _                    = []

-- 3ª definición (con concat y comprensión):
igualesAlSiguiente3 :: Eq a => [a] -> [a]
igualesAlSiguiente3 xs = concat [ys | (_:ys) <- group xs]

-- 4ª definición (con concat y map):
igualesAlSiguiente4 :: Eq a => [a] -> [a]
igualesAlSiguiente4 xs = concat (map tail (group xs))

-- 5ª definición (con =<<):
igualesAlSiguiente5 :: Eq a => [a] -> [a]
igualesAlSiguiente5 xs = tail =<< (group xs)

-- Nota: En la definición anterior se usa el operador (=<<) ya que
--    f =<< xs
-- es equivalente a
--    concat (map f xs)
-- Por ejemplo,
--    ghci> tail =<< [[1],[2,3,4],[9,7],[6]]
--    [3,4,7]
--    ghci> init =<< [[1],[2,3,4],[9,7],[6]]
--    [2,3,9]
--    ghci> reverse =<< [[1],[2,3,4],[9,7],[6]]
--    [1,4,3,2,7,9,6]
--    ghci> (take 2) =<< [[1],[2,3,4],[9,7],[6]]
--    [1,2,3,9,7,6]
--    ghci> (drop 2) =<< [[1],[2,3,4],[9,7],[6]]
--    [4]
--    ghci> (++[0]) =<< [[1],[2,3,4],[3,7],[6]]
--    [1,0,2,3,4,0,3,7,0,6,0]

-- 6ª definición (con =<< y sin argumentos):
igualesAlSiguiente6 :: Eq a => [a] -> [a]
igualesAlSiguiente6 = (tail =<<) . group

-- 7ª definición (con concatMap):
igualesAlSiguiente7 :: Eq a => [a] -> [a]
igualesAlSiguiente7 xs = concatMap tail (group xs)

-- Nota: En la definición anterior se usa la función ya que
--    concatMap f xs
-- es equivalente a
--    concat (map f xs)
-- Por ejemplo,
--    ghci> concatMap tail [[1],[2,3,4],[9,7],[6]]
--    [3,4,7]

-- 8ª definición (con concatMap y sin argumentos):
igualesAlSiguiente8 :: Eq a => [a] -> [a]
igualesAlSiguiente8 = (concatMap tail) . group

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int])  -> Spec
specG igualesAlSiguiente = do
  it "e1" $
    igualesAlSiguiente [1,2,2,2,3,3,4]  `shouldBe`  [2,2,3]
  it "e2" $
    igualesAlSiguiente [1..10] `shouldBe` []

spec :: Spec
spec = do
  describe "def. 1" $ specG igualesAlSiguiente1
  describe "def. 2" $ specG igualesAlSiguiente2
  describe "def. 3" $ specG igualesAlSiguiente3
  describe "def. 4" $ specG igualesAlSiguiente4
  describe "def. 5" $ specG igualesAlSiguiente5
  describe "def. 6" $ specG igualesAlSiguiente6
  describe "def. 7" $ specG igualesAlSiguiente7
  describe "def. 8" $ specG igualesAlSiguiente8

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_igualesAlSiguiente :: [Int] -> Bool
prop_igualesAlSiguiente xs =
  all (== igualesAlSiguiente1 xs)
      [igualesAlSiguiente2 xs,
       igualesAlSiguiente3 xs,
       igualesAlSiguiente4 xs,
       igualesAlSiguiente5 xs,
       igualesAlSiguiente6 xs,
       igualesAlSiguiente7 xs,
       igualesAlSiguiente8 xs]

-- La comprobación es
--    λ> quickCheck prop_igualesAlSiguiente
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    > ej = concatMap show [1..10^6]
--    (0.01 secs, 446,752 bytes)
--    λ> length ej
--    5888896
--    (0.16 secs, 669,787,856 bytes)
--    λ> length (show (igualesAlSiguiente1 ej))
--    588895
--    (1.60 secs, 886,142,944 bytes)
--    λ> length (show (igualesAlSiguiente2 ej))
--    588895
--    (1.95 secs, 1,734,143,816 bytes)
--    λ> length (show (igualesAlSiguiente3 ej))
--    588895
--    (1.81 secs, 1,178,232,104 bytes)
--    λ> length (show (igualesAlSiguiente4 ej))
--    588895
--    (1.43 secs, 1,932,010,304 bytes)
--    λ> length (show (igualesAlSiguiente5 ej))
--    588895
--    (0.40 secs, 2,016,810,320 bytes)
--    λ> length (show (igualesAlSiguiente6 ej))
--    588895
--    (0.32 secs, 1,550,409,984 bytes)
--    λ> length (show (igualesAlSiguiente7 ej))
--    588895
--    (0.34 secs, 1,550,410,104 bytes)
--    λ> length (show (igualesAlSiguiente8 ej))
--    588895
--    (0.33 secs, 1,550,410,024 bytes)
