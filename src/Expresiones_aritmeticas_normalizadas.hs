-- Expresiones_aritmeticas_normalizadas.hs
-- Expresiones aritmética normalizadas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-Diciembre-2014 (actualizado 25-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El siguiente tipo de dato representa expresiones construidas con
-- variables, sumas y productos
--    data Expr = Var String
--              | S Expr Expr
--              | P Expr Expre
-- Por ejemplo, x*(y+z) se representa por (P (V "x") (S (V "y") (V "z")))
--
-- Una expresión es un término si es un producto de variables. Por
-- ejemplo, x*(y*z) es un término pero x+(y*z) ni x*(y+z) lo son.
--
-- Una expresión está en forma normal si es una suma de términos. Por
-- ejemplo, x*(y*z) y x+(y*z) está en forma normal; pero x*(y+z) y
-- (x+y)*(x+z) no lo están.
--
-- Definir las funciones
--    esTermino, esNormal :: Expr -> Bool
-- tales que
-- + (esTermino a) se verifica si a es un término. Por ejemplo,
--      esTermino (V "x")                         == True
--      esTermino (P (V "x") (P (V "y") (V "z"))) == True
--      esTermino (P (V "x") (S (V "y") (V "z"))) == False
--      esTermino (S (V "x") (P (V "y") (V "z"))) == False
-- + (esNormal a) se verifica si a estña en forma normal. Por ejemplo,
--      esNormal (V "x")                                     == True
--      esNormal (P (V "x") (P (V "y") (V "z")))             == True
--      esNormal (S (V "x") (P (V "y") (V "z")))             == True
--      esNormal (P (V "x") (S (V "y") (V "z")))             == False
--      esNormal (P (S (V "x") (V "y")) (S (V "y") (V "z"))) == False
--      esNormal (S (P (V "x") (V "y")) (S (V "z") (V "x"))) == True
-- ---------------------------------------------------------------------

module Expresiones_aritmeticas_normalizadas where

import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck

data Expr = V String
          | S Expr Expr
          | P Expr Expr
  deriving Show

-- Definición de esTermino
-- =======================

esTermino :: Expr -> Bool
esTermino (V _)   = True
esTermino (S _ _) = False
esTermino (P a b) = esTermino a && esTermino b

-- 1ª definición de esNormal
-- =========================

esNormal1 :: Expr -> Bool
esNormal1 (S a b) = esNormal1 a && esNormal1 b
esNormal1 a       = esTermino a

-- 2ª definición de esNormal
-- =========================

-- (sumandos e) es la lista de los sumandos de la expresión e. Por ejemplo,
--    λ> sumandos (S (P (V "x") (V "y")) (S (V "z") (V "x")))
--    [P (V "x") (V "y"),V "z",V "x"]
sumandos :: Expr -> [Expr]
sumandos (S x y) = sumandos x ++ sumandos y
sumandos x       = [x]

esNormal2 :: Expr -> Bool
esNormal2 expr = all esTermino (sumandos expr)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    esTermino (V "x")                         `shouldBe` True
  it "e2" $
    esTermino (P (V "x") (P (V "y") (V "z"))) `shouldBe` True
  it "e3" $
    esTermino (P (V "x") (S (V "y") (V "z"))) `shouldBe` False
  it "e4" $
    esTermino (S (V "x") (P (V "y") (V "z"))) `shouldBe` False
  it "e5" $
    esNormal1 (V "x")                                     `shouldBe` True
  it "e6" $
    esNormal1 (P (V "x") (P (V "y") (V "z")))             `shouldBe` True
  it "e7" $
    esNormal1 (S (V "x") (P (V "y") (V "z")))             `shouldBe` True
  it "e9" $
    esNormal1 (P (V "x") (S (V "y") (V "z")))             `shouldBe` False
  it "e9" $
    esNormal1 (P (S (V "x") (V "y")) (S (V "y") (V "z"))) `shouldBe` False
  it "e10" $
    esNormal1 (S (P (V "x") (V "y")) (S (V "z") (V "x"))) `shouldBe` True
  it "e5" $
    esNormal2 (V "x")                                     `shouldBe` True
  it "e6" $
    esNormal2 (P (V "x") (P (V "y") (V "z")))             `shouldBe` True
  it "e7" $
    esNormal2 (S (V "x") (P (V "y") (V "z")))             `shouldBe` True
  it "e9" $
    esNormal2 (P (V "x") (S (V "y") (V "z")))             `shouldBe` False
  it "e9" $
    esNormal2 (P (S (V "x") (V "y")) (S (V "y") (V "z"))) `shouldBe` False
  it "e10" $
    esNormal2 (S (P (V "x") (V "y")) (S (V "z") (V "x"))) `shouldBe` True

-- La verificación es
--    λ> verifica
--    16 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- genVar es un generador de variables. Por ejemplo,
--    λ> generate genVar
--    "f"
--    λ> generate genVar
--    "g"
--    genVar :: Gen String
genVar :: Gen String
genVar = (: []) <$> choose ('a', 'z')

-- (exprArbitraria) es una expresión aleatoria de profundidad cercana a
-- n. Por ejemplo,
--    λ> generate (exprArbitraria 3)
--    S (V "o") (P (V "z") (V "a"))
--    λ> generate (exprArbitraria 3)
--    V "k"
exprArbitraria :: Int -> Gen Expr
exprArbitraria 0 = V <$> genVar
exprArbitraria n = oneof [
  V <$> genVar,
  S <$> subExpr <*> subExpr,
  P <$> subExpr <*> subExpr
  ]
  where subExpr = exprArbitraria (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized exprArbitraria

-- La propiedad es
prop_esNormal :: Expr -> Bool
prop_esNormal e =
  esNormal1 e == esNormal2 e

-- La comprobación es
--    λ> quickCheck prop_esNormal
--    +++ OK, passed 100 tests.
