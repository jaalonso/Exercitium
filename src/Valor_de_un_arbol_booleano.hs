-- Valor_de_un_arbol_booleano.hs
-- Valor de un árbol booleano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-enero-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran los árboles con operaciones booleanas definidos por
--    data Arbol = H Bool
--               | Conj Arbol Arbol
--               | Disy Arbol Arbol
--               | Neg Arbol
--
-- Por ejemplo, los árboles
--                Conj                            Conj
--               /   \                           /   \
--              /     \                         /     \
--           Disy      Conj                  Disy      Conj
--          /   \       /  \                /   \      /   \
--       Conj    Neg   Neg True          Conj    Neg   Neg  True
--       /  \    |     |                 /  \    |     |
--    True False False False          True False True  False
--
-- se definen por
--    ej1, ej2:: Arbol
--    ej1 = Conj (Disy (Conj (H True) (H False))
--                     (Neg (H False)))
--               (Conj (Neg (H False))
--                     (H True))
--
--    ej2 = Conj (Disy (Conj (H True) (H False))
--                     (Neg (H True)))
--               (Conj (Neg (H False))
--                     (H True))
--
-- Definir la función
--    valor :: Arbol -> Bool
-- tal que (valor ar) es el resultado de procesar el árbol realizando
-- las operaciones booleanas especificadas en los nodos. Por ejemplo,
--    valor ej1 == True
--    valor ej2 == False
-- ---------------------------------------------------------------------

module Valor_de_un_arbol_booleano where

data Arbol = H Bool
           | Conj Arbol Arbol
           | Disy Arbol Arbol
           | Neg Arbol

ej1, ej2 :: Arbol
ej1 = Conj (Disy (Conj (H True) (H False))
                 (Neg (H False)))
           (Conj (Neg (H False))
                 (H True))

ej2 = Conj (Disy (Conj (H True) (H False))
                 (Neg (H True)))
           (Conj (Neg (H False))
                 (H True))

valor :: Arbol -> Bool
valor (H x)      = x
valor (Neg a)    = not (valor a)
valor (Conj i d) = valor i && valor d
valor (Disy i d) = valor i || valor d
