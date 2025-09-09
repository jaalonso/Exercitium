-- Divide_si_todos_multiplos.hs
-- Divide si todos son múltiplos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-Julio-2014 (actualizado 9-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divideSiTodosMultiplos :: Integral a => a -> [a] -> Maybe [a]
-- tal que (divideSiTodosMultiplos x ys) es justo la lista de los
-- cocientes de los elementos de ys entre x si todos son múltiplos de x
-- y Nothing en caso contrario (donde x es distinto de cero). Por ejemplo,
--    divideSiTodosMultiplos 2 [6,10,4]  ==  Just [3,5,2]
--    divideSiTodosMultiplos 2 [6,10,5]  ==  Nothing
--
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Divide_si_todos_multiplos where

import Data.Maybe (isNothing, fromJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

divideSiTodosMultiplos1 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos1 x ys
  | todosMultiplos x ys = Just [y `div` x | y <- ys]
  | otherwise           = Nothing

-- (todosMultiplos x ys) se verifica si todos los elementos de ys son
-- múltiplos de x. Por ejemplo,
--    todosMultiplos 2 [6,10,4]  ==  True
--    todosMultiplos 2 [6,10,5]  ==  False
todosMultiplos :: Integral a => a -> [a] -> Bool
todosMultiplos x ys =
  and [y `mod` x == 0 | y <- ys]

-- 2ª solución
-- ===========

divideSiTodosMultiplos2 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos2 x ys
  | todosMultiplos2 x ys = Just [y `div` x | y <- ys]
  | otherwise            = Nothing

todosMultiplos2 :: Integral a => a -> [a] -> Bool
todosMultiplos2 x =
  all (\y -> y `mod` x == 0)

-- 3ª solución
-- ===========

divideSiTodosMultiplos3 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos3 0 _  = Nothing
divideSiTodosMultiplos3 _ [] = Just []
divideSiTodosMultiplos3 x (y:ys)
  | y `mod` x /= 0  = Nothing
  | isNothing aux   = Nothing
  | otherwise       = Just ((y `div` x) : fromJust aux)
  where aux = divideSiTodosMultiplos3 x ys

-- 4ª solución
-- ===========

divideSiTodosMultiplos4 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos4 0 _  = Nothing
divideSiTodosMultiplos4 _ [] = Just []
divideSiTodosMultiplos4 x (y:ys)
  | y `mod` x /= 0        = Nothing
  | otherwise = case divideSiTodosMultiplos4 x ys of
                  Nothing   -> Nothing
                  Just qs   -> Just (y `div` x : qs)

-- 5ª solución
-- ===========

divideSiTodosMultiplos5 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos5 x ys =
  sequence (map (x `divide`) ys)

-- (divide x y) es justo el cociente de x entre y, si x es divisible por
-- y y Nothing, en caso contrario. Por ejemplo,
divide :: Integral a => a -> a -> Maybe a
divide x y
  | y `mod` x == 0 = Just (y `div` x)
  | otherwise      = Nothing

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

divideSiTodosMultiplos6 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos6 x =
  sequence . map (x `divide`)

-- 7ª solución
-- ===========

divideSiTodosMultiplos7 :: Integral a => a -> [a] -> Maybe [a]
divideSiTodosMultiplos7 x =
  mapM (x `divide`)

-- Nota. En la solución anterior se usa la función mapM ya que
--    mapM f
-- es equivalente a
--    sequence . map f
-- Por ejemplo,
--    λ> mapM (\n -> if even n then Just (2*n) else Nothing) [4,6,10]
--    Just [8,12,20]
--    λ> mapM (\n -> if even n then Just (2*n) else Nothing) [4,6,11]
--    Nothing

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [Int] -> Maybe [Int]) -> Spec
specG divideSiTodosMultiplos = do
  it "e1" $
    divideSiTodosMultiplos 2 [6,10,4]  `shouldBe`  Just [3,5,2]
  it "e2" $
    divideSiTodosMultiplos 2 [6,10,5]  `shouldBe`  Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG divideSiTodosMultiplos1
  describe "def. 2" $ specG divideSiTodosMultiplos2
  describe "def. 3" $ specG divideSiTodosMultiplos3
  describe "def. 4" $ specG divideSiTodosMultiplos4
  describe "def. 5" $ specG divideSiTodosMultiplos5
  describe "def. 6" $ specG divideSiTodosMultiplos6
  describe "def. 7" $ specG divideSiTodosMultiplos7

-- La verificación es
--    λ> verifica
--    21 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_divideSiTodosMultiplos :: NonZero Int -> [Int] -> Bool
prop_divideSiTodosMultiplos x' ys =
  all (== divideSiTodosMultiplos1 x ys)
      [divideSiTodosMultiplos2 x ys,
       divideSiTodosMultiplos3 x ys,
       divideSiTodosMultiplos4 x ys,
       divideSiTodosMultiplos5 x ys,
       divideSiTodosMultiplos6 x ys]
  where x = getNonZero x'

-- La comprobación es
--    λ> quickCheck prop_divideSiTodosMultiplos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last <$> (divideSiTodosMultiplos1 2 [2,4..10^6])
--    Just 500000
--    (0.40 secs, 284,605,936 bytes)
--    λ> last <$> (divideSiTodosMultiplos2 2 [2,4..10^6])
--    Just 500000
--    (0.32 secs, 212,605,792 bytes)
--    λ> last <$> (divideSiTodosMultiplos3 2 [2,4..10^6])
--    Just 500000
--    (1.30 secs, 595,295,824 bytes)
--    λ> last <$> (divideSiTodosMultiplos4 2 [2,4..10^6])
--    Just 500000
--    (0.98 secs, 462,908,920 bytes)
--    λ> last <$> (divideSiTodosMultiplos5 2 [2,4..10^6])
--    Just 500000
--    (0.73 secs, 405,609,672 bytes)
--    λ> last <$> (divideSiTodosMultiplos6 2 [2,4..10^6])
--    Just 500000
--    (0.81 secs, 405,609,800 bytes)
--    λ> last <$> (divideSiTodosMultiplos7 2 [2,4..10^6])
--    Just 500000
--    (0.72 secs, 377,609,624 bytes)
