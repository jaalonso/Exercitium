-- Variables_de_una_formula.hs
-- El tipo de las fórmulas proposicionales: Variables de una fórmula
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de las fórmulas proposicionales se puede definir por
--    data FProp = Const Bool
--               | Var Char
--               | Neg FProp
--               | Conj FProp FProp
--               | Impl FProp FProp
--      deriving Show
-- de modo que la fórmula A → ⊥ ∧ ¬B se representa por
--    Impl (Var 'A') (Conj (Const False) (Neg (Var 'B')))
--
-- Definir la función
--    variables :: FProp -> [Char]
-- tal que (variables p) es la lista de las variables de la fórmula
-- p. Por ejemplo,
--    λ> variables (Impl (Var 'A') (Conj (Const False) (Neg (Var 'B'))))
--    "AB"
--    λ> variables (Impl (Var 'A') (Conj (Var 'A') (Neg (Var 'B'))))
--    "AAB"
-- ---------------------------------------------------------------------

module Variables_de_una_formula where

data FProp = Const Bool
           | Var Char
           | Neg FProp
           | Conj FProp FProp
           | Impl FProp FProp
  deriving Show

variables :: FProp -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q
