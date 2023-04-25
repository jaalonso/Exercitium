-- Arboles_balanceados.hs
-- Árboles balanceados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Diremos que un árbol está balanceado si para cada nodo la diferencia
-- entre el número de nodos de sus subárboles izquierdo y derecho es
-- menor o igual que uno.
--
-- Usando el [tipo de los árboles binarios con valores en los nodos]
-- (https://bit.ly/40Pplzj), definir la función
--    balanceado :: Arbol a -> Bool
-- tal que (balanceado a) se verifica si el árbol a está balanceado. Por
-- ejemplo,
--    λ> balanceado (N 5 H (N 3 H H))
--    True
--    λ> balanceado (N 4 (N 3 (N 2 H H) H) (N 5 H (N 6 H (N 7 H H))))
--    False
-- ---------------------------------------------------------------------

module Arboles_balanceados where

import Arbol_binario_valores_en_nodos (Arbol (H, N))

balanceado :: Arbol a -> Bool
balanceado H         = True
balanceado (N _ i d) = abs (numeroNodos i - numeroNodos d) <= 1
                       && balanceado i
                       && balanceado d

-- (numeroNodos a) es el número de nodos del árbol a. Por ejemplo,
--    numeroNodos (N 5 H (N 3 H H)) ==  2
numeroNodos :: Arbol a -> Int
numeroNodos H         = 0
numeroNodos (N _ i d) = 1 + numeroNodos i + numeroNodos d
