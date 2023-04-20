-- Valor_de_la_resta.hs
-- El tipo de las expresiones aritméticas: Valor de la resta.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las expresiones aritméticas](https://bit.ly/40vCQUh),
-- definir la función
--    resta :: Expr -> Expr -> Expr
-- tal que (resta e1 e2) es la expresión correspondiente a la diferencia
-- de e1 y e2. Por ejemplo,
--    resta (Lit 42) (Lit 2)  ==  Suma (Lit 42) (Op (Lit 2))
--
-- Comprobar con QuickCheck que
--    valor (resta x y) == valor x - valor y
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Valor_de_la_resta where

import Tipo_expresion_aritmetica (Expr (..))
import Valor_de_una_expresion_aritmetica (valor)
import Test.QuickCheck

resta :: Expr -> Expr -> Expr
resta x y = Suma x (Op y)

-- Comprobación de la propiedad
-- ============================

-- (exprArbitraria n) es una expresión aleatoria de tamaño n. Por
-- ejemplo,
--    λ> sample (exprArbitraria 3)
--    Op (Op (Lit 0))
--    SiCero (Lit 0) (Lit (-2)) (Lit (-1))
--    Op (Suma (Lit 3) (Lit 0))
--    Op (Lit 5)
--    Op (Lit (-1))
--    Op (Op (Lit 9))
--    Suma (Lit (-12)) (Lit (-12))
--    Suma (Lit (-9)) (Lit 10)
--    Op (Suma (Lit 8) (Lit 15))
--    SiCero (Lit 16) (Lit 9) (Lit (-5))
--    Suma (Lit (-3)) (Lit 1)
exprArbitraria :: Int -> Gen Expr
exprArbitraria n
  | n <= 1 = Lit <$> arbitrary
  | otherwise = oneof
                [ Lit <$> arbitrary
                , let m = div n 2
                  in Suma <$> exprArbitraria m <*> exprArbitraria m
                , Op <$> exprArbitraria (n - 1)
                , let m = div n 3
                  in SiCero <$> exprArbitraria m
                            <*> exprArbitraria m
                            <*> exprArbitraria m ]

-- Expr es subclase de Arbitrary
instance Arbitrary Expr where
  arbitrary = sized exprArbitraria


-- La propiedad es
prop_resta :: Expr -> Expr -> Property
prop_resta x y =
  valor (resta x y) === valor x - valor y

-- La comprobación es
--    λ> quickCheck prop_resta
--    +++ OK, passed 100 tests.
