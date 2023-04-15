-- Variables_de_una_formula.hs
-- El tipo de las fórmulas proposicionales: Variables de una fórmula
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el tipo de las fórmulas proposicionales definido en el
-- [ejercicio anterior](https://bit.ly/3L3G2SX), definir la función
--    variables :: FProp -> [Char]
-- tal que (variables p) es la lista de las variables de la fórmula
-- p. Por ejemplo,
--    λ> variables (Impl (Var 'A') (Conj (Const False) (Neg (Var 'B'))))
--    "AB"
--    λ> variables (Impl (Var 'A') (Conj (Var 'A') (Neg (Var 'B'))))
--    "AAB"
-- ---------------------------------------------------------------------

module Variables_de_una_formula where

import Tipo_de_formulas (FProp(..))

variables :: FProp -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q
