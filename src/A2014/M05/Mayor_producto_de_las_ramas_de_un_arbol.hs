-- Mayor_producto_de_las_ramas_de_un_arbol.hs
-- Mayor producto de las ramas de un árbol.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-mayo-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--       1              3
--      / \            /|\
--     2   3          / | \
--         |         5  4  7
--         4         |     /\
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
--
-- Definir la función
--    mayorProducto :: (Ord a, Num a) => Arbol a -> a
-- tal que (mayorProducto a) es el mayor producto de las ramas del árbol
-- a. Por ejemplo,
--    λ> mayorProducto (N 1 [N 2 [], N  3 []])
--    3
--    λ> mayorProducto (N 1 [N 8 [], N  4 [N 3 []]])
--    12
--    λ> mayorProducto (N 1 [N 2 [],N 3 [N 4 []]])
--    12
--    λ> mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    90
--    λ> mayorProducto (N (-8) [N 0 [N (-9) []],N 6 []])
--    0
--    λ> a = N (-4) [N (-7) [],N 14 [N 19 []],N (-1) [N (-6) [],N 21 []],N (-4) []]
--    λ> mayorProducto a
--    84
-- ---------------------------------------------------------------------

module A2014.M05.Mayor_producto_de_las_ramas_de_un_arbol where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Arbol a = N a [Arbol a]
  deriving Show

-- 1ª solución
-- ===========

mayorProducto1 :: (Ord a, Num a) => Arbol a -> a
mayorProducto1 a = maximum [product xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    λ> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- 2ª solución
-- ===========

mayorProducto2 :: (Ord a, Num a) => Arbol a -> a
mayorProducto2 a = maximum (map product (ramas a))

-- 3ª solución
-- ===========

mayorProducto3 :: (Ord a, Num a) => Arbol a -> a
mayorProducto3 = maximum . map product . ramas

-- 4º solución
-- ===========

mayorProducto4 :: (Ord a, Num a) => Arbol a -> a
mayorProducto4 = maximum . productosRamas

-- (productosRamas a) es la lista de los productos de las ramas
-- del árbol a. Por ejemplo,
--    λ> productosRamas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [90,12,42,21]
productosRamas :: (Ord a, Num a) => Arbol a -> [a]
productosRamas (N x []) = [x]
productosRamas (N x xs) = [x * y | a <- xs, y <- productosRamas a]

-- 5ª solución
-- ===========

mayorProducto5 :: (Ord a, Num a) => Arbol a -> a
mayorProducto5 (N x []) = x
mayorProducto5 (N x xs)
  | x > 0     = x * maximum (map mayorProducto5 xs)
  | x == 0    = 0
  | otherwise = x * minimum (map menorProducto xs)

-- (menorProducto a) es el menor producto de las ramas del árbol
-- a. Por ejemplo,
--    λ> menorProducto (N 1 [N 2 [], N  3 []])
--    2
--    λ> menorProducto (N 1 [N 8 [], N  4 [N 3 []]])
--    8
--    λ> menorProducto (N 1 [N 2 [],N 3 [N 4 []]])
--    2
--    λ> menorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    12
menorProducto :: (Ord a, Num a) => Arbol a -> a
menorProducto (N x []) = x
menorProducto (N x xs)
  | x > 0     = x * minimum (map menorProducto xs)
  | x == 0    = 0
  | otherwise = x * maximum (map mayorProducto5 xs)

-- 6ª solución
-- ===========

mayorProducto6 :: (Ord a, Num a) => Arbol a -> a
mayorProducto6 = maximum . aux
  where aux (N a []) = [a]
        aux (N a b)  = [v,u]
          where u = maximum g
                v = minimum g
                g = map (*a) (concatMap aux b)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Arbol Int -> Int) -> Spec
specG mayorProducto = do
  it "e1" $
    mayorProducto (N 1 [N 2 [], N  3 []]) `shouldBe`3
  it "e2" $
    mayorProducto (N 1 [N 8 [], N  4 [N 3 []]]) `shouldBe`12
  it "e3" $
    mayorProducto (N 1 [N 2 [],N 3 [N 4 []]]) `shouldBe`12
  it "e4" $
    mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
    `shouldBe` 90
  it "e5" $
    mayorProducto (N (-8) [N 0 [N (-9) []],N 6 []]) `shouldBe` 0
  it "e6" $ do
    let a = N (-4) [N (-7) [],N 14 [N 19 []],N (-1) [N (-6) [],N 21 []],N (-4) []]
    mayorProducto a `shouldBe` 84

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorProducto1
  describe "def. 2" $ specG mayorProducto2
  describe "def. 3" $ specG mayorProducto3
  describe "def. 4" $ specG mayorProducto4
  describe "def. 5" $ specG mayorProducto5
  describe "def. 6" $ specG mayorProducto6

-- La verificación es
--    λ> verifica
--    36 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- (arbolArbitrario n) es un árbol aleatorio de orden n. Por ejemplo,
--   > sample (arbolArbitrario 5 :: Gen (Arbol Int))
--   N 0 [N 0 []]
--   N (-2) []
--   N 4 []
--   N 2 [N 4 []]
--   N 8 []
--   N (-2) [N (-9) [],N 7 []]
--   N 11 []
--   N (-11) [N 4 [],N 14 []]
--   N 10 [N (-3) [],N 13 []]
--   N 12 [N 11 []]
--   N 20 [N (-18) [],N (-13) []]
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
prop_mayorProducto :: Arbol Integer -> Bool
prop_mayorProducto a =
  all (== mayorProducto1 a)
      [f a | f <- [ mayorProducto2
                  , mayorProducto3
                  , mayorProducto4
                  , mayorProducto5
                  , mayorProducto6
                  ]]

-- La comprobación es
--    λ> quickCheck prop_mayorProducto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ejArbol <- generate (arbolArbitrario 600 :: Gen (Arbol Integer))
--    λ> mayorProducto1 ejArbol
--    2419727651266241493467136000
--    (1.87 secs, 1,082,764,480 bytes)
--    λ> mayorProducto2 ejArbol
--    2419727651266241493467136000
--    (1.57 secs, 1,023,144,008 bytes)
--    λ> mayorProducto3 ejArbol
--    2419727651266241493467136000
--    (1.55 secs, 1,023,144,248 bytes)
--    λ> mayorProducto4 ejArbol
--    2419727651266241493467136000
--    (1.60 secs, 824,473,800 bytes)
--    λ> mayorProducto5 ejArbol
--    2419727651266241493467136000
--    (0.83 secs, 732,370,352 bytes)
--    λ> mayorProducto6 ejArbol
--    2419727651266241493467136000
--    (0.98 secs, 817,473,344 bytes)
--
--    λ> ejArbol2 <- generate (arbolArbitrario 700 :: Gen (Arbol Integer))
--    λ> mayorProducto5 ejArbol2
--    1044758937398026715504640000000
--    (4.94 secs, 4,170,324,376 bytes)
--    λ> mayorProducto6 ejArbol2
--    1044758937398026715504640000000
--    (5.88 secs, 4,744,782,024 bytes)
