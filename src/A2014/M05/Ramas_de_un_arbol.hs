-- Ramas_de_un_arbol.hs
-- Ramas de un árbol.
-- José A. Alonso <https://jaalonso.github.io>
-- Sevilla, 9-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\
--    2   3           / | \
--        |          5  4  7
--        4          |     /\
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]
--
-- Definir la función
--    ramas :: Arbol b -> [[b]]
-- tal que (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ramas ej1  ==  [[1,2],[1,3,4]]
--    ramas ej2  ==  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module A2014.M05.Ramas_de_un_arbol where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
-- ===========

ramas1 :: Arbol b -> [[b]]
ramas1 (N x []) = [[x]]
ramas1 (N x as) = [x : xs | a <- as, xs <- ramas1 a]

-- 2ª solución
-- ===========

ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concat (map (map (x:)) (map ramas2 as))

-- 3ª solución
-- ===========

ramas3 :: Arbol b -> [[b]]
ramas3 (N x []) = [[x]]
ramas3 (N x as) = concat (map (map (x:) . ramas3) as)

-- 4ª solución
-- ===========

ramas4 :: Arbol b -> [[b]]
ramas4 (N x []) = [[x]]
ramas4 (N x as) = concatMap (map (x:) . ramas4) as

-- 5ª solución
-- ===========

ramas5 :: Arbol a -> [[a]]
ramas5 (N x []) = [[x]]
ramas5 (N x xs) = map ramas5 xs >>= map (x:)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Arbol Int -> [[Int]]) -> Spec
specG ramas = do
  it "e1" $
    ramas ej1  `shouldBe`  [[1,2],[1,3,4]]
  it "e2" $
    ramas ej2  `shouldBe`  [[3,5,6],[3,4],[3,7,2],[3,7,1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG ramas1
  describe "def. 2" $ specG ramas2
  describe "def. 3" $ specG ramas3
  describe "def. 4" $ specG ramas4
  describe "def. 5" $ specG ramas5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de la equivalencia de las definiciones
-- ===================================================

-- (arbolArbitrario n) es un árbol aleatorio de orden n. Por ejemplo,
--    λ> sample (arbolArbitrario 4 :: Gen (Arbol Int))
--    N 0 [N 0 []]
--    N 1 [N 1 [N (-2) [N (-1) [N (-1) [N (-1) [N 1 []]]]]],N (-1) [N 2 []]]
--    N 1 [N (-2) [],N 0 [N (-4) [N (-2) []]]]
--    N (-4) [N 1 [],N 0 [N 6 [N (-4) []],N 2 [N 3 []]]]
--    N (-7) [N (-7) [N (-3) []]]
--    N (-2) [N (-8) []]
--    N (-3) [N 3 [N 2 []]]
--    N (-12) [N 5 [],N 0 []]
--    N 14 [N 13 [N (-12) []],N 11 [],N 8 [N (-13) []]]
--    N (-12) [N (-6) [N 16 [N (-14) [N (-1) []]]]]
--    N (-5) []
arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n = do
  x  <- arbitrary
  ms <- sublistOf [0 .. n `div` 2]
  as <- mapM arbolArbitrario ms
  return (N x as)

-- Arbol es una subclase de Arbitraria
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario

-- La propiedad es
prop_arbol :: Arbol Int -> Bool
prop_arbol a =
  all (== ramas1 a)
      [ramas2 a,
       ramas3 a,
       ramas4 a,
       ramas5 a]

-- La comprobación es
--    λ> quickCheck prop_arbol
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ej600 <- generate (arbolArbitrario 600 :: Gen (Arbol Int))
--    λ> length (ramas1 ej600)
--    1262732
--    (1.92 secs, 1,700,238,488 bytes)
--    λ> length (ramas2 ej600)
--    1262732
--    (1.94 secs, 2,549,877,280 bytes)
--    λ> length (ramas3 ej600)
--    1262732
--    (1.99 secs, 2,446,508,472 bytes)
--    λ> length (ramas4 ej600)
--    1262732
--    (1.67 secs, 2,090,469,104 bytes)
--    λ> length (ramas5 ej600)
--    1262732
--    (1.66 secs, 2,112,198,232 bytes)
