-- Arboles_con_la_misma_forma.hs
-- Árboles con la misma forma.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-diciembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo de los árboles binarios con los valores en las hojas]
-- (https://bit.ly/3N5RuyE), definir la función
--    mismaForma :: Arbol a -> Arbol b -> Bool
-- tal que (mismaForma t1 t2) se verifica si t1 y t2 tienen la misma
-- estructura. Por ejemplo,
--    λ> arbol1 = Hoja 5
--    λ> arbol2 = Hoja 3
--    λ> mismaForma arbol1 arbol2
--    True
--    λ> arbol3 = Nodo (Hoja 6) (Hoja 7)
--    λ> mismaForma arbol1 arbol3
--    False
--    λ> arbol4 = Nodo (Hoja 9) (Hoja 5)
--    λ> mismaForma arbol3 arbol4
--    True
-- ---------------------------------------------------------------------

module Arboles_con_la_misma_forma where

import Arbol_binario_valores_en_hojas (Arbol (..))
import Aplicacion_de_una_funcion_a_un_arbol (mapArbol)
import Test.QuickCheck

-- 1ª solución
-- ===========

mismaForma1 :: Arbol a -> Arbol b -> Bool
mismaForma1 (Hoja _)   (Hoja _)     = True
mismaForma1 (Nodo l r) (Nodo l' r') = mismaForma1 l l' && mismaForma1 r r'
mismaForma1 _          _            = False

-- 2ª solución
-- ===========

mismaForma2 :: Arbol a -> Arbol b -> Bool
mismaForma2 x y = f x == f y
  where
    f = mapArbol (const ())

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mismaForma :: Arbol Int -> Arbol Int -> Property
prop_mismaForma a1 a2 =
  mismaForma1 a1 a2 === mismaForma2 a1 a2

-- La comprobación es
--    λ> quickCheck prop_mismaForma
--    +++ OK, passed 100 tests.
