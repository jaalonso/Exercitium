-- Interpretaciones_de_una_formula.hs
-- El tipo de las fórmulas proposicionales: Interpretaciones de una fórmula
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 02-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de las fórmulas proposicionales](https://bit.ly/3L3G2SX),
-- definir la función
--    interpretaciones :: FProp -> [Interpretacion]
-- tal que (interpretaciones p) es la lista de las interpretaciones de
-- la fórmula p. Por ejemplo,
--    λ> interpretaciones (Impl (Var 'A') (Conj (Var 'A') (Var 'B')))
--    [[('A',False),('B',False)],
--     [('A',False),('B',True)],
--     [('A',True),('B',False)],
--     [('A',True),('B',True)]]
-- ---------------------------------------------------------------------

module Interpretaciones_de_una_formula where

import Tipo_de_formulas (FProp(..))
import Variables_de_una_formula (variables)
import Valor_de_una_formula (Interpretacion)
import Data.List (nub)

interpretaciones :: FProp -> [Interpretacion]
interpretaciones p =
  [zip vs i | i <- interpretacionesVar (length vs)]
  where vs = nub (variables p)

-- (interpretacionesVar n) es la lista de las interpretaciones de n
-- variables. Por ejemplo,
--    λ> interpretacionesVar 2
--    [[False,False],
--     [False,True],
--     [True,False],
--     [True,True]]
interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0 = [[]]
interpretacionesVar n = map (False:) bss ++ map (True:) bss
  where bss = interpretacionesVar (n-1)
