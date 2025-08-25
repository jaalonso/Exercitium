-- Suma_si_todos_justos.hs
-- Suma si todos los valores son justos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-Mayo-2014 (Revisión del 25-Agosto-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaSiTodosJustos :: (Num a, Eq a) => [Maybe a] -> Maybe a
-- tal que (sumaSiTodosJustos xs) es justo la suma de todos los
-- elementos de xs si todos son justos (es decir, si Nothing no
-- pertenece a xs) y Nothing en caso contrario. Por ejemplo,
--    sumaSiTodosJustos [Just 2, Just 5]           == Just 7
--    sumaSiTodosJustos [Just 2, Just 5, Nothing]  == Nothing
-- ---------------------------------------------------------------------

module Suma_si_todos_justos where

import Data.Maybe (catMaybes, isJust, fromJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

sumaSiTodosJustos1 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos1 xs
  | todosJustos xs = Just (sum [x | (Just x) <- xs])
  | otherwise      = Nothing

-- (todosJustos xs) se verifica si todos los elementos de xs son justos
-- (es decir, si Nothing no pertenece a xs) y Nothing en caso
-- contrario. Por ejemplo,
--    todosJustos [Just 2, Just 5]           == True
--    todosJustos [Just 2, Just 5, Nothing]  == False

-- 1ª definición de todosJustos:
todosJustos1 :: Eq a => [Maybe a] -> Bool
todosJustos1 = notElem Nothing

-- 2ª definición de todosJustos:
todosJustos :: Eq a => [Maybe a] -> Bool
todosJustos = all isJust

-- 2ª solución
-- ===========

sumaSiTodosJustos2 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos2 xs
  | todosJustos xs = Just (sum [fromJust x | x <- xs])
  | otherwise      = Nothing

-- 3ª solución
-- ===========

sumaSiTodosJustos3 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos3 xs
  | todosJustos xs = Just (sum (map fromJust xs))
  | otherwise      = Nothing

-- 4ª solución

sumaSiTodosJustos4 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos4 xs
  | todosJustos xs = Just (sum (catMaybes xs))
  | otherwise      = Nothing

-- 5ª solución
-- ===========

sumaSiTodosJustos5 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos5 xs = suma (sequence xs)
  where suma Nothing   = Nothing
        suma (Just ys) = Just (sum ys)

-- Nota. En la solución anterior se usa la función
--    sequence :: Monad m => [m a] -> m [a]
-- tal que (sequence xs) es la mónada obtenida evaluando cada una de las
-- de xs de izquierda a derecha. Por ejemplo,
--    sequence [Just 2, Just 5]   ==  Just [2,5]
--    sequence [Just 2, Nothing]  ==  Nothing
--    sequence [[2,4],[5,7]]      ==  [[2,5],[2,7],[4,5],[4,7]]
--    sequence [[2,4],[5,7],[6]]  ==  [[2,5,6],[2,7,6],[4,5,6],[4,7,6]]
--    sequence [[2,4],[5,7],[]]   ==  []

-- 6ª solución
-- ===========

sumaSiTodosJustos6 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos6 xs = fmap sum (sequence xs)

-- 7ª solución
-- ===========

sumaSiTodosJustos7 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos7 = fmap sum . sequence

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Maybe Integer] -> Maybe Integer) -> Spec
specG sumaSiTodosJustos = do
  it "e1" $
    sumaSiTodosJustos [Just 2, Just 5]           `shouldBe` Just 7
  it "e2" $
    sumaSiTodosJustos [Just 2, Just 5, Nothing]  `shouldBe` Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaSiTodosJustos1
  describe "def. 2" $ specG sumaSiTodosJustos2
  describe "def. 3" $ specG sumaSiTodosJustos3
  describe "def. 4" $ specG sumaSiTodosJustos4
  describe "def. 5" $ specG sumaSiTodosJustos5
  describe "def. 6" $ specG sumaSiTodosJustos6
  describe "def. 7" $ specG sumaSiTodosJustos7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_sumaSiTodosJustos :: [Maybe Integer] -> Bool
prop_sumaSiTodosJustos xs =
  all (== sumaSiTodosJustos1 xs)
      [sumaSiTodosJustos2 xs,
       sumaSiTodosJustos3 xs,
       sumaSiTodosJustos4 xs,
       sumaSiTodosJustos5 xs,
       sumaSiTodosJustos6 xs,
       sumaSiTodosJustos7 xs]

verifica_sumaSiTodosJustos :: IO ()
verifica_sumaSiTodosJustos =
  quickCheck prop_sumaSiTodosJustos

-- La comprobación es
--    λ> verifica_sumaSiTodosJustos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

ejemplo1 :: Integer -> [Maybe Integer]
ejemplo1 n = map Just [1..n]

ejemplo2 :: Integer -> [Maybe Integer]
ejemplo2 n = map Just [1..n] ++ [Nothing]

-- La comparación es
--    λ> sumaSiTodosJustos1 (ejemplo1 10000000)
--    Just 50000005000000
--    (4.08 secs, 3,520,610,288 bytes)
--    λ> sumaSiTodosJustos2 (ejemplo1 10000000)
--    Just 50000005000000
--    (4.75 secs, 4,000,610,376 bytes)
--    λ> sumaSiTodosJustos3 (ejemplo1 10000000)
--    Just 50000005000000
--    (2.55 secs, 3,680,610,368 bytes)
--    λ> sumaSiTodosJustos4 (ejemplo1 10000000)
--    Just 50000005000000
--    (2.87 secs, 3,360,610,184 bytes)
--    λ> sumaSiTodosJustos5 (ejemplo1 10000000)
--    Just 50000005000000
--    (4.96 secs, 3,461,220,272 bytes)
--    λ> sumaSiTodosJustos6 (ejemplo1 10000000)
--    Just 50000005000000
--    (3.30 secs, 3,461,220,152 bytes)
--    λ> sumaSiTodosJustos7 (ejemplo1 10000000)
--    Just 50000005000000
--    (4.10 secs, 3,461,220,304 bytes)
--
--    λ> sumaSiTodosJustos1 (ejemplo2 10000000)
--    Nothing
--    (3.22 secs, 3,200,600,632 bytes)
--    λ> sumaSiTodosJustos2 (ejemplo2 10000000)
--    Nothing
--    (2.12 secs, 3,200,600,688 bytes)
--    λ> sumaSiTodosJustos3 (ejemplo2 10000000)
--    Nothing
--    (3.22 secs, 3,200,600,704 bytes)
--    λ> sumaSiTodosJustos4 (ejemplo2 10000000)
--    Nothing
--    (2.34 secs, 3,200,600,616 bytes)
--    λ> sumaSiTodosJustos5 (ejemplo2 10000000)
--    Nothing
--    (4.02 secs, 3,061,210,672 bytes)
--    λ> sumaSiTodosJustos6 (ejemplo2 10000000)
--    Nothing
--    (2.81 secs, 3,061,210,568 bytes)
--    λ> sumaSiTodosJustos7 (ejemplo2 10000000)
--    Nothing
--    (2.61 secs, 3,061,210,720 bytes)
