-- ElementosMinimales.hs
-- Determinación de los elementos minimales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24 de abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    minimales :: Eq a => [[a]] -> [[a]]
-- tal que (minimales xss) es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
--    minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
--    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module A2014.M04.ElementosMinimales where

import Data.List (nub, delete)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

minimales :: Eq a => [[a]] -> [[a]]
minimales xss =
  [xs | xs <- xss,
        null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo,
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = subconjuntoPropio' (nub xs) (nub ys)
  where
    subconjuntoPropio' _ [] = False
    subconjuntoPropio' [] _ = True
    subconjuntoPropio' (x:xs') ys' =
      x `elem` ys' && subconjuntoPropio xs' (delete x ys')

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  describe "def. 1" $ do
    it "e1" $
      minimales [[1,3],[2,3,1],[3,2,5]]        `shouldBe`  [[2,3,1],[3,2,5]]
    it "e2" $
      minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  `shouldBe`  [[2,3,1],[3,2,5]]

-- La verificación es
--    λ> verifica
--    2 examples, 0 failures
