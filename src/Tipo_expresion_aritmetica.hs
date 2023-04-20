-- Tipo_expresion_aritmetica.hs
-- El tipo de las expresiones aritméticas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de las expresiones aritméticas formadas por
-- + literales (p.e. Lit 7),
-- + sumas (p.e. Suma (Lit 7) (Suma (Lit 3) (Lit 5)))
-- + opuestos (p.e. Op (Suma (Op (Lit 7)) (Suma (Lit 3) (Lit 5))))
-- + expresiones condicionales (p.e. (SiCero (Lit 3) (Lit 4) (Lit 5))
-- se define como se muestra a continuación.

module Tipo_expresion_aritmetica where

data Expr = Lit Int
          | Suma Expr Expr
          | Op Expr
          | SiCero Expr Expr Expr
  deriving (Eq, Show)
