-- Sustitucion_en_una_expresion_aritmetica.hs
-- Sustitución en una expresión aritmética.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 24-Julio-2014 (actualizado 10-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La expresiones aritméticas se pueden representar mediante el
-- siguiente tipo
--    data Expr = C Int
--              | V Char
--              | S Expr Expr
--              | P Expr Expr
--      deriving (Eq, Show)
-- por ejemplo, la expresión "z*(3+x)" se representa por
-- (P (V 'z') (S (C 3) (V 'x'))).
--
-- Definir la función
--    sustitucion :: Expr -> [(Char, Int)] -> Expr
-- tal que (sustitucion e s) es la expresión obtenida sustituyendo las
-- variables de la expresión e según se indica en la sustitución s. Por
-- ejemplo,
--    λ> sustitucion (P (V 'z') (S (C 3) (V 'x'))) [('x',7),('z',9)]
--    P (C 9) (S (C 3) (C 7))
--    λ> sustitucion (P (V 'z') (S (C 3) (V 'y'))) [('x',7),('z',9)]
--    P (C 9) (S (C 3) (V 'y'))
-- ---------------------------------------------------------------------

module Sustitucion_en_una_expresion_aritmetica where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Expr = C Int
          | V Char
          | S Expr Expr
          | P Expr Expr
  deriving (Eq, Show)

-- 1ª solución
-- ===========

sustitucion1 :: Expr -> [(Char, Int)] -> Expr
sustitucion1 e []         = e
sustitucion1 (V c) ((d,n):ps)
  | c == d                = C n
  | otherwise             = sustitucion1 (V c) ps
sustitucion1 (C n) _      = C n
sustitucion1 (S e1 e2) ps = S (sustitucion1 e1 ps) (sustitucion1 e2 ps)
sustitucion1 (P e1 e2) ps = P (sustitucion1 e1 ps) (sustitucion1 e2 ps)

-- 2ª solución
-- ===========

sustitucion2 :: Expr -> [(Char, Int)] -> Expr
sustitucion2 (V c) s = case lookup c s of
                        Just n  -> C n
                        Nothing -> V c
sustitucion2 (C n) _ = C n
sustitucion2 (S e1 e2) s = S (sustitucion2 e1 s) (sustitucion2 e2 s)
sustitucion2 (P e1 e2) s = P (sustitucion2 e1 s) (sustitucion2 e2 s)

-- 3ª solución
-- ===========

sustitucion3 :: Expr -> [(Char, Int)] -> Expr
sustitucion3 (V c) s     = maybe (V c) C (lookup c s)
sustitucion3 (C n) _     = C n
sustitucion3 (S e1 e2) s = S (sustitucion3 e1 s) (sustitucion3 e2 s)
sustitucion3 (P e1 e2) s = P (sustitucion3 e1 s) (sustitucion3 e2 s)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Expr -> [(Char, Int)] -> Expr) -> Spec
specG sustitucion = do
  it "e1" $
    sustitucion (P (V 'z') (S (C 3) (V 'x'))) [('x',7),('z',9)]
    `shouldBe` P (C 9) (S (C 3) (C 7))
  it "e2" $
    sustitucion (P (V 'z') (S (C 3) (V 'y'))) [('x',7),('z',9)]
    `shouldBe` P (C 9) (S (C 3) (V 'y'))

spec :: Spec
spec = do
  describe "def. 1" $ specG sustitucion1
  describe "def. 2" $ specG sustitucion2
  describe "def. 3" $ specG sustitucion3

-- La verificación es
--    λ> verifica
  --    6 examples, 0 failures


-- Comprobación de equivalencia
-- ============================

-- Generador de caracteres solo entre 'a' y 'z'
genVar :: Gen Char
genVar = elements ['a'..'z']

-- (exprArbitraria n) es un generador de expresiones de tamaño
-- aproximado a n. Por
-- ejemplo,
--    λ> generate (exprArbitraria 4)
--    S (S (P (V 'l') (V 'e')) (S (V 'p') (V 'j'))) (V 'm')
exprArbitraria :: Int -> Gen Expr
exprArbitraria 0 = oneof [
  V <$> genVar,
  C <$> arbitrary
  ]
exprArbitraria n = oneof [
  V <$> genVar,
  C <$> arbitrary,
  S <$> subExpr <*> subExpr,
  P <$> subExpr <*> subExpr
  ]
  where subExpr = exprArbitraria (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized exprArbitraria

-- La propiedad es
prop_sustitucion :: Expr -> [(Char, Int)] -> Bool
prop_sustitucion e s =
  all (== sustitucion1 e s)
      [sustitucion2 e s,
       sustitucion3 e s
      ]

-- La comprobación es
--    λ> quickCheck prop_sustitucion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- (exprArbitrariaProf n) es un generador de expresiones de tamaño
-- aproximado a n. Por ejemplo,
--    λ> generate (exprArbitrariaProf 3)
--    S (S (S (C 5) (C 1)) (S (C 11) (V 's'))) (S (S (V 't') (C (-13))) (S (V 'a') (V 'r')))
exprArbitrariaProf :: Int -> Gen Expr
exprArbitrariaProf 0 = oneof [
  V <$> genVar,
  C <$> arbitrary
  ]
exprArbitrariaProf n = oneof [
  S <$> exprArbitrariaProf (n-1) <*> exprArbitrariaProf (n-1),
  P <$> exprArbitrariaProf (n-1) <*> exprArbitrariaProf (n-1)
  ]

-- La comparación es
--    λ> ej <- generate (exprArbitrariaProf 20)
--    λ> let r = sustitucion1 ej (zip ['a'..'z'] [1..]) in r == r
--    True
--    (7.19 secs, 2,905,973,720 bytes)
--    λ> let r = sustitucion2 ej (zip ['a'..'z'] [1..]) in r == r
--    True
--    (1.75 secs, 470,342,024 bytes)
--    λ> let r = sustitucion3 ej (zip ['a'..'z'] [1..]) in r == r
--    True
--    (1.73 secs, 482,919,872 bytes)
