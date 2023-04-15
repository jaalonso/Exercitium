-- Tipo_de_formulas.py
-- El tipo de las fórmulas proposicionales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-noviembre-2022
-- ======================================================================

-- La fórmula A → ⊥ ∧ ¬B se representa por
--    Impl(Var('A'), Conj(Const(False), Neg (Var('B'))))
-- usando el tipo de las fórmulas proposicionales definido como se
-- muestra a continuación.

module Tipo_de_formulas where

data FProp = Const Bool
           | Var Char
           | Neg FProp
           | Conj FProp FProp
           | Impl FProp FProp
  deriving Show
