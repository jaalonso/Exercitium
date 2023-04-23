-- Arbol_binario_valores_en_hojas.hs
-- El tipo de los árboles binarios con valores en las hojas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-diciembre-2022
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
-- usando el tipo de los árboles binarios con valores en las hojas
-- definido como se muestra a continuación.

module Arbol_binario_valores_en_hojas where

import Test.QuickCheck

data Arbol a = Hoja a
             | Nodo (Arbol a) (Arbol a)
  deriving (Eq, Show)

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
