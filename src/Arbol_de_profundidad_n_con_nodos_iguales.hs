-- Arbol_de_profundidad_n_con_nodos_iguales.hs
-- Árbol de profundidad n con nodos iguales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios](https://bit.ly/3H53exA),
-- definir las funciones
--    repeatArbol    :: a -> Arbol a
--    replicateArbol :: Int -> a -> Arbol a
-- tales que
-- + (repeatArbol x) es es árbol con infinitos nodos x. Por ejemplo,
--      takeArbol 0 (repeatArbol 3) == H 3
--      takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--      takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- + (replicate n x) es el árbol de profundidad n cuyos nodos son x. Por
--   ejemplo,
--      replicateArbol 0 5  ==  H 5
--      replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--      replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
--
-- Comprobar con QuickCheck que el número de hojas de
-- (replicateArbol n x) es 2^n, para todo número natural n.
-- ---------------------------------------------------------------------

module Arbol_de_profundidad_n_con_nodos_iguales where

import Arboles_binarios (Arbol (..))
import Numero_de_hojas_de_un_arbol_binario (nHojas)
import Test.QuickCheck

repeatArbol :: a -> Arbol a
repeatArbol x = N x t t
  where t = repeatArbol x

replicateArbol :: Int -> a -> Arbol a
replicateArbol n = takeArbol n . repeatArbol

-- (takeArbol n t) es el subárbol de t de profundidad n. Por ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H x)     = H x
takeArbol 0 (N x _ _) = H x
takeArbol n (N x i d) = N x (takeArbol (n-1) i) (takeArbol (n-1) d)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_replicateArbol :: Int -> Int -> Property
prop_replicateArbol n x =
  n >= 0 ==> nHojas (replicateArbol n x) == 2^n

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_replicateArbol
--    +++ OK, passed 100 tests.
