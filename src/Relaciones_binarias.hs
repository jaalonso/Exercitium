-- Relaciones_binarias.hs
-- Relaciones binarias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una relación binaria R sobre un conjunto A se puede representar
-- mediante un par (xs,ps) donde xs es la lista de los elementos de A
-- (el universo de R) y ps es la lista de pares de elementos de xs (el
-- grafo de R).
--
-- Definir el tipo de dato (Rel a), para representar las relaciones
-- binarias sobre a, y la función
--    esRelacionBinaria :: Eq a => Rel a -> Bool
-- tal que (esRelacionBinaria r) se verifica si r es una relación
-- binaria. Por ejemplo,
--    λ> esRelacionBinaria (R ([1, 3], [(3, 1), (3, 3)]))
--    True
--    λ> esRelacionBinaria (R ([1, 3], [(3, 1), (3, 2)]))
--    False
-- ---------------------------------------------------------------------

module Relaciones_binarias where

import Data.List (nub)
import Test.QuickCheck

newtype Rel a = R ([a], [(a,a)])
  deriving Show

-- 1ª solución
-- ===========

esRelacionBinaria :: Eq a => Rel a -> Bool
esRelacionBinaria (R (u, g)) =
  and [x `elem` u && y `elem` u | (x,y) <- g]

-- 2ª solución
-- ===========

esRelacionBinaria2 :: Eq a => Rel a -> Bool
esRelacionBinaria2 (R (u, g)) =
  all (\(x,y) -> x `elem` u && y `elem` u) g

-- 3ª solución
-- ===========

esRelacionBinaria3 :: Eq a => Rel a -> Bool
esRelacionBinaria3 (R (_, []))         = True
esRelacionBinaria3 (R (u, (x,y):g)) =
  x `elem` u &&
  y `elem` u &&
  esRelacionBinaria3 (R (u, g))

-- Comprobación de equivalencia
-- ============================

-- Generador de relaciones binarias. Por ejemplo,
--    λ> sample (relacionArbitraria :: Gen (Rel Int))
--    R ([0],[])
--    R ([0,-1,1],[(0,-1),(0,1),(-1,1),(1,0)])
--    R ([1],[])
--    R ([-5,3],[(-5,-5),(-5,3),(3,-5),(3,3)])
--    R ([-2,-7],[(-7,-7)])
--    R ([11,-7],[])
--    R ([0],[])
--    R ([-13,-11],[(-13,-13)])
relacionArbitraria :: (Arbitrary a, Eq a) => Gen (Rel a)
relacionArbitraria = do
  n <- choose (0, 10)
  u1 <- vectorOf n arbitrary
  let u = nub u1
  g <- sublistOf [(x,y) | x <- u, y <- u]
  return (R (u, g))

-- Relaciones es una subclase de Arbitrary.
instance (Arbitrary a, Eq a) => Arbitrary (Rel a) where
  arbitrary = relacionArbitraria

-- La propiedad es
prop_esRelacionBinaria :: Rel Int -> Bool
prop_esRelacionBinaria r =
  esRelacionBinaria r  &&
  esRelacionBinaria2 r &&
  esRelacionBinaria3 r

-- La comprobación es
--    λ> quickCheck prop_esRelacionBinaria
--    +++ OK, passed 100 tests.
