-- Arboles_con_la_misma_forma.hs
-- Árboles con la misma forma.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El árbol binario
--         ·
--        / \
--       /   \
--      ·     ·
--     / \   / \
--    1   4 6   9
-- se puede representar por
--    ejArbol = Nodo (Nodo (Hoja 1) (Hoja 4))
--                   (Nodo (Hoja 6) (Hoja 9))
--
-- El tipo de los árboles binarios se puede definir por
--    data Arbol a = Hoja a
--                 | Nodo (Arbol a) (Arbol a)
--      deriving (Show, Eq)
--
-- Definir la función
--    mismaForma :: Arbol a -> Arbol b -> Bool
-- tal que (mismaForma t1 t2) se verifica si t1 y t2 tienen la misma
-- estructura. Por ejemplo,
--    λ> arbol1 = Hoja 5
--    λ> arbol2 = Hoja 3
--    λ> mismaForma1 arbol1 arbol2
--    True
--    λ> arbol3 = Nodo (Hoja 6) (Hoja 7)
--    λ> mismaForma1 arbol1 arbol3
--    False
--    λ> arbol4 = Nodo (Hoja 9) (Hoja 5)
--    λ> mismaForma1 arbol3 arbol4
--    True
-- ---------------------------------------------------------------------

module Arboles_con_la_misma_forma where

import Test.QuickCheck

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- 1ª solución
-- ===========

mismaForma1 :: Arbol a -> Arbol b -> Bool
mismaForma1 (Hoja _)   (Hoja _)     = True
mismaForma1 (Nodo l r) (Nodo l' r') = mismaForma1 l l' && mismaForma1 r r'
mismaForma1 _          _            = False

-- 2ª solución
-- ===========

mismaForma2 :: Arbol a -> Arbol b -> Bool
mismaForma2 x y = f x == f y
  where
    f = mapArbol (const ())

-- (mapArbol f t) es el árbolo obtenido aplicando la función f a los
-- elementos del árbol t. Por ejemplo,
--    λ> mapArbol (+ 1) (Nodo (Hoja 2) (Hoja 4))
--    Nodo (Hoja 3) (Hoja 5)
mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol f (Hoja a)   = Hoja (f a)
mapArbol f (Nodo i d) = Nodo (mapArbol f i) (mapArbol f d)

-- Comprobación de equivalencia
-- ============================

-- (arbolArbitrario n) es un árbol aleatorio de altura n. Por ejemplo,
--    λ> sample (arbolArbitrario 3 :: Gen (Arbol Int))
--    Nodo (Nodo (Nodo (Hoja 0) (Hoja 0)) (Hoja 0)) (Hoja 0)
--    Nodo (Nodo (Hoja 4) (Hoja 8)) (Hoja (-4))
--    Nodo (Nodo (Nodo (Hoja 4) (Hoja 10)) (Hoja (-6))) (Hoja (-1))
--    Nodo (Nodo (Hoja 3) (Hoja 6)) (Hoja (-5))
--    Nodo (Nodo (Hoja (-11)) (Hoja (-13))) (Hoja 14)
--    Nodo (Nodo (Hoja (-7)) (Hoja 15)) (Hoja (-2))
--    Nodo (Nodo (Hoja (-9)) (Hoja (-2))) (Hoja (-6))
--    Nodo (Nodo (Hoja (-15)) (Hoja (-16))) (Hoja (-20))
arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n
  | n <= 1    = Hoja <$> arbitrary
  | otherwise = do
      k <- choose (2, n - 1)
      Nodo <$> arbolArbitrario k <*> arbolArbitrario (n - k)

-- Arbol es subclase de Arbitraria
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario
  shrink (Hoja x)   = Hoja <$> shrink x
  shrink (Nodo l r) = l :
                      r :
                      [Nodo l' r | l' <- shrink l] ++
                      [Nodo l r' | r' <- shrink r]

-- La propiedad es
prop_mismaForma :: Arbol Int -> Arbol Int -> Property
prop_mismaForma a1 a2 =
  mismaForma1 a1 a2 === mismaForma2 a1 a2

-- La comprobación es
--    λ> quickCheck prop_mismaForma
--    +++ OK, passed 100 tests.
