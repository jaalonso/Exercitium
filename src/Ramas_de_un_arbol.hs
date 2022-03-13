-- Ramas_de_un_arbol.hs
-- Ramas de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--                   deriving Show
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

module Ramas_de_un_arbol where

import Test.QuickCheck

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
ramas1 :: Arbol b -> [[b]]
ramas1 (N x []) = [[x]]
ramas1 (N x as) = [x : xs | a <- as, xs <- ramas1 a]

-- 2ª solución
ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concat (map (map (x:)) (map ramas2 as))

-- 3ª solución
ramas3 :: Arbol b -> [[b]]
ramas3 (N x []) = [[x]]
ramas3 (N x as) = concatMap (map (x:)) (map ramas3 as)

-- 4ª solución
ramas4 :: Arbol a -> [[a]]
ramas4 (N x []) = [[x]]
ramas4 (N x xs) = map ramas4 xs >>= map (x:)

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
       ramas4 a]

-- La comprobación es
--    λ> quickCheck prop_arbol
--    +++ OK, passed 100 tests.
