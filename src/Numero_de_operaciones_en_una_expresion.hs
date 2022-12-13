-- Numero_de_operaciones_en_una_expresion.hs
-- El tipo de las expresiones aritméticas: Número de operaciones en una expresión.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se considera el tipo de las expresiones aritméticas definido por
--    data Expr = Lit Int
--              | Suma Expr Expr
--              | Op Expr
--              | SiCero Expr Expr Expr
--      deriving (Eq, Show)
-- formado por
-- + literales (p.e. Lit 7),
-- + sumas (p.e. Suma (Lit 7) (Suma (Lit 3) (Lit 5)))
-- + opuestos (p.e. Op (Suma (Op (Lit 7)) (Suma (Lit 3) (Lit 5))))
-- + expresiones condicionales (p.e. (SiCero (Lit 3) (Lit 4) (Lit 5))
--
-- Definir la función
--    numeroOps :: Expr -> Int
-- tal que (numeroOps e) es el número de operaciones de e. Por ejemplo,
--    numeroOps (Lit 3)                      ==  0
--    numeroOps (Suma (Lit 7) (Op (Lit 5)))  ==  2
-- ---------------------------------------------------------------------

module Numero_de_operaciones_en_una_expresion where

data Expr = Lit Int
          | Suma Expr Expr
          | Op Expr
          | SiCero Expr Expr Expr
  deriving (Eq, Show)

numeroOps :: Expr -> Int
numeroOps (Lit _)        = 0
numeroOps (Suma x y)     = 1 + numeroOps x + numeroOps y
numeroOps (Op x)         = 1 + numeroOps x
numeroOps (SiCero x y z) = 1 + numeroOps x + numeroOps y + numeroOps z
