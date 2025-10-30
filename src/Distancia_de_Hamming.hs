-- Distancia_de_Hamming.hs
-- Distancia de Hamming.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-Noviembre-2014 (actualizado 30-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La distancia de Hamming entre dos listas es el número de posiciones
-- en que los correspondientes elementos son distintos. Por ejemplo, la
-- distancia de Hamming entre "roma" y "loba" es 2 (porque hay 2
-- posiciones en las que los elementos correspondientes son distintos:
-- la 1ª y la 3ª).
--
-- Definir la función
--    distancia :: Eq a => [a] -> [a] -> Int
-- tal que (distancia xs ys) es la distancia de Hamming entre xs e
-- ys. Por ejemplo,
--    distancia "romano" "comino"  ==  2
--    distancia "romano" "camino"  ==  3
--    distancia "roma"   "comino"  ==  2
--    distancia "roma"   "camino"  ==  3
--    distancia "romano" "ron"     ==  1
--    distancia "romano" "cama"    ==  2
--    distancia "romano" "rama"    ==  1
--
-- Comprobar con QuickCheck si la distancia de Hamming tiene la
-- siguiente propiedad
--    distancia(xs,ys) = 0 si, y sólo si, xs = ys
-- y, en el caso de que no se verifique, modificar ligeramente la
-- propiedad para obtener una condición necesaria y suficiente de
-- distancia(xs,ys) = 0.
-- ---------------------------------------------------------------------

module Distancia_de_Hamming where

import Data.List (foldl')
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

distancia1 :: Eq a => [a] -> [a] -> Int
distancia1 xs ys =
  sum [1 | (x,y) <- zip xs ys, x /= y]

-- 2ª solución
-- ===========

distancia2 :: Eq a => [a] -> [a] -> Int
distancia2 [] _  = 0
distancia2 _  [] = 0
distancia2 (x:xs) (y:ys)
  | x /= y    = 1 + distancia2 xs ys
  | otherwise = distancia2 xs ys

-- 3ª solución
-- ===========

distancia3 :: Eq a => [a] -> [a] -> Int
distancia3 xs ys =
  foldl' (\n (x, y) -> if x /= y then n + 1 else n) 0 (zip xs ys)

-- 4ª solución
-- ===========

distancia4 :: Eq a => [a] -> [a] -> Int
distancia4 xs ys =
  length (filter (uncurry (/=)) (zip xs ys))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> String -> Int) -> Spec
specG distancia = do
  it "e1" $
    distancia "romano" "comino"  `shouldBe`  2
  it "e2" $
    distancia "romano" "camino"  `shouldBe`  3
  it "e3" $
    distancia "roma"   "comino"  `shouldBe`  2
  it "e4" $
    distancia "roma"   "camino"  `shouldBe`  3
  it "e5" $
    distancia "romano" "ron"     `shouldBe`  1
  it "e6" $
    distancia "romano" "cama"    `shouldBe`  2
  it "e7" $
    distancia "romano" "rama"    `shouldBe`  1

spec :: Spec
spec = do
  describe "def. 1" $ specG distancia1
  describe "def. 2" $ specG distancia2
  describe "def. 3" $ specG distancia3
  describe "def. 4" $ specG distancia4

-- La verificación es
--    λ> verifica
--    35 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_distancia :: String -> String -> Bool
prop_distancia xs ys =
  all (== distancia1 xs ys)
      [distancia2 xs ys,
       distancia3 xs ys,
       distancia4 xs ys]

-- La comprobación es
--    λ> quickCheck prop_distancia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> distancia1 [1..10^7] [1,3..10^7]
--    4999999
--    (1.92 secs, 1,960,602,616 bytes)
--    λ> distancia2 [1..10^7] [1,3..10^7]
--    4999999
--    (3.87 secs, 2,012,653,888 bytes)
--    λ> distancia3 [1..10^7] [1,3..10^7]
--    4999999
--    (1.81 secs, 2,120,602,496 bytes)
--    λ> distancia4 [1..10^7] [1,3..10^7]
--    4999999
--    (0.42 secs, 1,680,602,600 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_distancia1 :: [Int] -> [Int] -> Bool
prop_distancia1 xs ys =
  (distancia1 xs ys == 0) == (xs == ys)

-- La comprobación es
--    λ> quickCheck prop_distancia1
--    *** Failed! Falsifiable (after 2 tests and 1 shrink):
--    []
--    [1]
-- En efecto,
--    λ> distancia [] [1] == 0
--    True
--    λ> [] == [1]
--    False

-- La primera modificación es restringir la propiedad a lista de igual
-- longitud:
prop_distancia2 :: [Int] -> [Int] -> Property
prop_distancia2 xs ys =
  length xs == length ys ==>
  (distancia1 xs ys == 0) == (xs == ys)

-- La comprobación es
--    λ> quickCheck prop_distancia2
--    *** Gave up! Passed only 33 tests.

-- Nota. La propiedad se verifica, pero al ser la condición demasiado
-- restringida sólo 33 de los casos la cumple.

-- La segunda restricción es limitar las listas a la longitud de la más
-- corta:
prop_distancia3 :: [Int] -> [Int] -> Bool
prop_distancia3 xs ys =
  (distancia1 xs ys == 0) == (take n xs == take n ys)
  where n = min (length xs) (length ys)

-- La comprobación es
--    λ> quickCheck prop_distancia3
--    +++ OK, passed 100 tests.
