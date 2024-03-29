-- Duplicacion_de_cada_elemento.hs
-- Duplicación de cada elemento
-- José A. Alonso Jiménez
-- Sevilla, 24-febrero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    duplicaElementos :: [a] -> [a]
-- tal que (duplicaElementos xs) es la lista obtenida duplicando cada
-- elemento de xs. Por ejemplo,
--    duplicaElementos [3,2,5]    ==  [3,3,2,2,5,5]
--    duplicaElementos "Haskell"  ==  "HHaasskkeellll"
-- ---------------------------------------------------------------------

module Duplicacion_de_cada_elemento where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

--  1ª solución
duplicaElementos1 :: [a] -> [a]
duplicaElementos1 []     = []
duplicaElementos1 (x:xs) = x : x : duplicaElementos1 xs

-- 2 solución
duplicaElementos2 :: [a] -> [a]
duplicaElementos2 = foldr (\x ys -> x:x:ys) []

-- 3ª solución
duplicaElementos3 :: [a] -> [a]
duplicaElementos3 xs = concat [[x,x] | x <- xs]

-- 4ª solución
duplicaElementos4 :: [a] -> [a]
duplicaElementos4 xs = concat (map (replicate 2) xs)

-- 5ª solución
duplicaElementos5 :: [a] -> [a]
duplicaElementos5 = concatMap (replicate 2)

-- 6ª solución
duplicaElementos6 :: [a] -> [a]
duplicaElementos6 = (>>= replicate 2)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int]) -> Spec
specG duplicaElementos = do
  it "e1" $
    duplicaElementos [3,2,5] `shouldBe` [3,3,2,2,5,5]

spec :: Spec
spec = do
  describe "def. 1" $ specG duplicaElementos1
  describe "def. 2" $ specG duplicaElementos2
  describe "def. 3" $ specG duplicaElementos3
  describe "def. 4" $ specG duplicaElementos4
  describe "def. 5" $ specG duplicaElementos5
  describe "def. 6" $ specG duplicaElementos6

-- La verificación es
--    λ> verifica
--
--    6 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_duplicaElementos :: [Int] -> Bool
prop_duplicaElementos xs =
  all (== duplicaElementos1 xs)
      [f xs | f <- [duplicaElementos2,
                    duplicaElementos3,
                    duplicaElementos4,
                    duplicaElementos5,
                    duplicaElementos6]]

verifica_duplicaElementos :: IO ()
verifica_duplicaElementos = quickCheck prop_duplicaElementos

-- La comprobación es
--    λ> verifica_duplicaElementos
--    +++ OK, passed 100 tests.
