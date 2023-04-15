-- Validez_de_una_formula.hs
-- Reconocedor de tautologías.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una fórmula es una tautología si es verdadera en todas sus
-- interpretaciones. Por ejemplo,
-- + (A ∧ B) → A es una tautología
-- + A → (A ∧ B) no es una tautología
--
-- Usando el tipo de las fórmulas proposicionales definido en el
-- [ejercicio anterior](https://bit.ly/3L3G2SX), definir la función
--    esTautologia :: FProp -> Bool
-- tal que (esTautologia p) se verifica si la fórmula p es una
-- tautología. Por ejemplo,
--    λ> esTautologia (Impl (Conj (Var 'A') (Var 'B')) (Var 'A'))
--    True
--    λ> esTautologia (Impl (Var 'A') (Conj (Var 'A') (Var 'B')))
--    False
-- ---------------------------------------------------------------------

module Validez_de_una_formula where

import Tipo_de_formulas (FProp(..))
import Valor_de_una_formula (valor)
import Interpretaciones_de_una_formula (interpretaciones)

esTautologia :: FProp -> Bool
esTautologia p =
  and [valor i p | i <- interpretaciones p]
