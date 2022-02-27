-- Suma_si_todos_justos.hs
-- Suma si todos los valores son justos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-marzo-2022
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
