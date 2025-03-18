-- Suma_si_todos_justos.hs
-- Suma si todos los valores son justos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-mayo-2014
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

module A2014.M05.Suma_si_todos_justos where

import Data.Maybe (catMaybes, fromJust, isJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

sumaSiTodosJustos1 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos1 xs
    | todosJustos xs = Just (sum [x | (Just x) <- xs])
    | otherwise      = Nothing

-- (todosJustos xs) se verifica si todos los elementos de xs si todos
-- son justos (es decir, si Nothing no pertenece a xs) y Nothing en caso
-- contrario. Por ejemplo,
--    todosJustos [Just 2, Just 5]           == True
--    todosJustos [Just 2, Just 5, Nothing]  == False
todosJustos :: Eq a => [Maybe a] -> Bool
todosJustos = notElem Nothing

-- 2ª solución
-- ===========

sumaSiTodosJustos2 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos2 xs
  | todosJustos2 xs = Just (sum [x | (Just x) <- xs])
  | otherwise       = Nothing

todosJustos2 :: Eq a => [Maybe a] -> Bool
todosJustos2 = all isJust

-- 3ª solución
-- ===========

sumaSiTodosJustos3 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos3 xs
  | todosJustos xs = Just (sum [fromJust x | x <- xs])
  | otherwise      = Nothing

-- 4ª solución
-- ===========

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

-- 6ª solución
-- ===========

sumaSiTodosJustos6 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos6 xs = fmap sum (sequence xs)

-- Nota. En la solución anterior se usa la función
--    fmap :: (a -> b) -> f a -> f b
-- tal que (fmap f x) aplica la función f al functor x. Por ejemplo,
--    fmap (+2) (Just 3)  ==  Just 5
--    fmap (+2) Nothing   ==  Nothing

-- 7ª solución
-- ===========

sumaSiTodosJustos7 :: (Num a, Eq a) => [Maybe a] -> Maybe a
sumaSiTodosJustos7 = fmap sum . sequence

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Maybe Int] -> Maybe Int) -> Spec
specG sumaSiTodosJustos = do
  it "e1" $
    sumaSiTodosJustos [Just 2, Just 5] `shouldBe` Just 7
  it "e2" $
    sumaSiTodosJustos [Just 2, Just 5, Nothing] `shouldBe` Nothing

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
