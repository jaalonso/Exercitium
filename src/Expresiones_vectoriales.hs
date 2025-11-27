-- Expresiones_vectoriales.hs
-- Expresiones vectoriales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-Diciembre-2014 (actualizado 26-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El siguiente tipo de dato define las expresiones vectoriales formadas
-- por un vector, la suma de dos expresiones vectoriales o el producto
-- de un entero por una expresión vectorial.
--    data ExpV = Vec Int Int
--              | Sum ExpV ExpV
--              | Mul Int ExpV
--      deriving Show
--
-- Definir la función
--    valor :: ExpV -> (Int,Int)
-- tal que (valor e) es el valor de la expresión vectorial e. Por
-- ejemplo,
--    valor (Vec 1 2)                                  ==  (1,2)
--    valor (Sum (Vec 1 2 ) (Vec 3 4))                 ==  (4,6)
--    valor (Mul 2 (Vec 3 4))                          ==  (6,8)
--    valor (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         ==  (8,12)
--    valor (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  ==  (8,12)
-- ---------------------------------------------------------------------

module Expresiones_vectoriales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data ExpV = Vec Int Int
          | Sum ExpV ExpV
          | Mul Int ExpV
  deriving Show

-- 1ª solución
-- ===========

valor1 :: ExpV -> (Int,Int)
valor1 (Vec x y)   = (x,y)
valor1 (Sum e1 e2) = (x1+x2,y1+y2)
  where (x1,y1) = valor1 e1
        (x2,y2) = valor1 e2
valor1 (Mul n e)   = (n*x,n*y)
  where (x,y) = valor1 e

-- 2ª solución
-- ===========

valor2 :: ExpV -> (Int,Int)
valor2 (Vec a b)   = (a, b)
valor2 (Sum e1 e2) = suma (valor2 e1) (valor2 e2)
valor2 (Mul n e1)  = multiplica n (valor2 e1)

suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma (a,b) (c,d) = (a+c,b+d)

multiplica :: Int -> (Int, Int) -> (Int, Int)
multiplica n (a,b) = (n*a,n*b)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (ExpV -> (Int,Int)) -> Spec
specG valor = do
  it "e1" $
    valor (Vec 1 2)                                  `shouldBe`  (1,2)
  it "e2" $
    valor (Sum (Vec 1 2 ) (Vec 3 4))                 `shouldBe`  (4,6)
  it "e3" $
    valor (Mul 2 (Vec 3 4))                          `shouldBe`  (6,8)
  it "e4" $
    valor (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         `shouldBe`  (8,12)
  it "e5" $
    valor (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  `shouldBe`  (8,12)

spec :: Spec
spec = do
  describe "def. 1" $ specG valor1
  describe "def. 2" $ specG valor2

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures


-- Comprobación de equivalencia
-- ============================

-- Generador de expresiones vectoriales. Por ejemplo,
--    λ> generate (genExpV 3)
--    Vec 26 18
--    λ> generate (genExpV 3)
--    Sum (Vec 1 30) (Vec 24 (-8))
genExpV :: Int -> Gen ExpV
genExpV 0 = Vec <$> arbitrary <*> arbitrary
genExpV n = oneof [
    Vec <$> arbitrary <*> arbitrary,
    Sum <$> subGen <*> subGen,
    Mul <$> arbitrary <*> subGen ]
  where
    subGen = genExpV (n `div` 2)

--
instance Arbitrary ExpV where
  arbitrary = sized genExpV

-- La propiedad es
prop_equivalencia :: ExpV -> Bool
prop_equivalencia a =
  valor1 a == valor2 a

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.
