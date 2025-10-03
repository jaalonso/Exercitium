-- Insercion_en_arboles_binarios_de_busqueda.hs
-- Inserción en árboles binarios de búsqueda
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 17-Julio-2014 (actualizado 1-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un árbol binario de búsqueda (ABB) es un árbol binario tal que el de
-- cada nodo es mayor que los valores de su subárbol izquierdo y es
-- menor que los valores de su subárbol derecho y, además, ambos
-- subárboles son árboles binarios de búsqueda. Por ejemplo, al
-- almacenar los valores de [8,4,2,6,3] en un ABB se puede obtener el
-- siguiente ABB:
--
--       3
--      / \
--     /   \
--    2     6
--         / \
--        4   8
--
-- Los ABB se pueden representar como tipo de dato algebraico:
--    data ABB = V
--      | N Int ABB ABB
--      deriving (Eq, Show)
-- Por ejemplo, la definición del ABB anteriore es
--    ej :: ABB
--    ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
--
-- Definir la función
--    inserta :: Int -> ABB -> ABB
-- tal que (inserta v a) es el árbol obtenido añadiendo el valor v al
-- ABB a, si no es uno de sus valores. Por ejemplo,
--    λ>  inserta 5 ej
--    N 3 (N 2 V V) (N 6 (N 4 V (N 5 V V)) (N 8 V V))
--    λ>  inserta 1 ej
--    N 3 (N 2 (N 1 V V) V) (N 6 (N 4 V V) (N 8 V V))
--    λ>  inserta 2 ej
--    N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
--
-- Comprobar con QuickCheck que al insertar un valor en un ABB se
-- obtiene otro ABB.
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Insercion_en_arboles_binarios_de_busqueda where

import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck

data ABB a = V
           | N a (ABB a) (ABB a)
  deriving (Show, Eq)

ej :: ABB Int
ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))

inserta :: Ord a => a -> ABB a -> ABB a
inserta v' V = N v' V V
inserta v' (N v i d)
  | v' == v   = N v i d
  | v' < v    = N v (inserta v' i) d
  | otherwise = N v i (inserta v' d)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    inserta 5 ej `shouldBe` N 3 (N 2 V V) (N 6 (N 4 V (N 5 V V)) (N 8 V V))
  it "e2" $
    inserta 1 ej `shouldBe` N 3 (N 2 (N 1 V V) V) (N 6 (N 4 V V) (N 8 V V))
  it "e3" $
    inserta 2 ej `shouldBe` N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))

-- La verificación es
--    λ> verifica
--    3 examples, 0 failures

-- Comprobación de la propiedad
-- ============================

-- (elementos a) es la lista de los valores de los nodos del ABB a en el
-- recorrido inorden. Por ejemplo,
--    elementos ej == [2,3,4,6,8]
elementos :: ABB a -> [a]
elementos V         = []
elementos (N v i d) = elementos i ++ [v] ++ elementos d

-- (menorTodos v a) se verifica si v es menor que todos los elementos
-- del ABB a. Por ejemplo,
--    menorTodos 1 ej == True
--    menorTodos 2 ej == False
menorTodos :: Ord a => a -> ABB a -> Bool
menorTodos _ V = True
menorTodos v a = v < minimum (elementos a)

-- (mayorTodos v a) se verifica si v es mayor que todos los elementos
-- del ABB a. Por ejemplo,
--    mayorTodos 9 ej == True
--    mayorTodos 8 ej == False
mayorTodos :: Ord a => a -> ABB a -> Bool
mayorTodos _ V = True
mayorTodos v a  = v > maximum (elementos a)

-- (esABB a) se verifica si a es un ABB correcto. Por ejemplo,
--    esABB ej == True
esABB :: Ord a => ABB a -> Bool
esABB V         = True
esABB (N v i d) = mayorTodos v i &&
                  menorTodos v d &&
                  esABB i &&
                  esABB d

-- vacio es el árbol binario de búsqueda vacío.
vacio :: ABB a
vacio = V

-- genABB es un generador de árboles binarios de búsqueda. Por ejemplo,
--    λ> generate genABB
--    N (-23) (N (-29) V V) (N (-3) V (N 6 V V))
--    λ> generate genABB
--    N (-24) V V
genABB :: Gen (ABB Int)
genABB = do
  xs <- listOf arbitrary
  return (foldr inserta vacio xs)

instance Arbitrary (ABB Int) where
  arbitrary = genABB

-- La propiedad es
prop_inserta :: Int -> ABB Int -> Bool
prop_inserta v a =
  esABB (inserta v a)

-- La comprobación es
--    λ> quickCheck prop_inserta
--    +++ OK, passed 100 tests.
