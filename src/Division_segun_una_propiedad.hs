-- Division_segun_una_propiedad.hs
-- División según una propiedad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-Enero-2015 (actualizado 17-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divideSegun :: (a -> Bool) -> [a] -> [[a]]
-- tal que (divideSegun p xs) es la lista de los segmentos de xs cuyos
-- elementos no cumplen la propiedad p. Por ejemplo,
--    divideSegun even [3,5,2,7,6,8,9,1]  ==  [[3,5],[7],[9,1]]
--    divideSegun odd  [3,5,2,7,6,8,9,1]  ==  [[2],[6,8]]
--
-- Comprobar con QuickCheck que, para cualquier lista xs de números
-- enteros, la concatenación de los elementos de (divideSegun even xs)
-- es la lista de los elementos de xs que son impares.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Division_segun_una_propiedad where

import Data.List (groupBy, unfoldr)
import Data.Function (on)
import Data.List.Split (splitWhen, wordsBy)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

divideSegun1 :: (a -> Bool) -> [a] -> [[a]]
divideSegun1 p xs
  | null ys   = []
  | otherwise = ys : divideSegun1 p zs
  where (ys,zs) = break p (dropWhile p xs)

-- 2ª solución
-- ===========

divideSegun2 :: (a -> Bool) -> [a] -> [[a]]
divideSegun2 p xs =
  case dropWhile p xs of
    []  -> []
    xs' -> let (ys, zs) = break p xs'
           in ys : divideSegun2 p zs

-- 3ª solución
-- ===========

divideSegun3 :: (a -> Bool) -> [a] -> [[a]]
divideSegun3 p = unfoldr f
  where
    f [] = Nothing
    f xs = case dropWhile p xs of
             [] -> Nothing
             xs' -> Just (break p xs')

-- 4ª solución
-- ===========

divideSegun4 :: (a -> Bool) -> [a] -> [[a]]
divideSegun4 p = filter (not . p . head) . groupBy ((==) `on` p)

-- 5ª solución
-- ===========

divideSegun5 :: (a -> Bool) -> [a] -> [[a]]
divideSegun5 p = filter (not . null) . splitWhen p

-- 6ª solución
-- ===========

divideSegun6 :: (a -> Bool) -> [a] -> [[a]]
divideSegun6 = wordsBy

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Bool) -> [Int] -> [[Int]]) -> Spec
specG divideSegun = do
  it "e1" $
    divideSegun even [3,5,2,7,6,8,9,1] `shouldBe` [[3,5],[7],[9,1]]
  it "e2" $
    divideSegun odd  [3,5,2,7,6,8,9,1] `shouldBe` [[2],[6,8]]

spec :: Spec
spec = do
  describe "def. 1" $ specG divideSegun1
  describe "def. 2" $ specG divideSegun2
  describe "def. 3" $ specG divideSegun3
  describe "def. 4" $ specG divideSegun4
  describe "def. 5" $ specG divideSegun5
  describe "def. 6" $ specG divideSegun6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: (Int -> Bool) -> [Int] -> Bool
prop_equivalencia p xs =
  all (== divideSegun1 p xs)
      [divideSegun2 p xs,
       divideSegun3 p xs,
       divideSegun4 p xs,
       divideSegun5 p xs,
       divideSegun6 p xs]

-- La comprobación es
--    λ> quickCheck' prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (divideSegun1 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (2.16 secs, 3,200,603,920 bytes)
--    λ> length (divideSegun2 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (1.75 secs, 3,147,269,168 bytes)
--    λ> length (divideSegun3 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (1.61 secs, 3,120,602,688 bytes)
--    λ> length (divideSegun4 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (2.01 secs, 5,613,935,888 bytes)
--    λ> length (divideSegun5 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (1.17 secs, 4,707,269,000 bytes)
--    λ> length (divideSegun6 even (take (10^7) (cycle [1,3,5,2,4,6])))
--    1666667
--    (1.11 secs, 4,760,602,176 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_divideSegun :: [Int] -> Bool
prop_divideSegun xs =
  concat (divideSegun1 even xs) == filter odd xs

-- La comprobación es
--    λ> quickCheck' prop_divideSegun
--    +++ OK, passed 100 tests.
