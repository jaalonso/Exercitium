-- TAD_Producto_cartesiano.hs
-- TAD de los conjuntos: TAD_Producto_cartesiano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-marzo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Utilizando el tipo abstracto de datos de los conjuntos
-- (https://bit.ly/3HbB7fo) definir la función
--    productoC :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
-- tal que (productoC c1 c2) es el producto cartesiano de los
-- conjuntos c1 y c2. Por ejemplo,
--    λ> ej1 = inserta 2 (inserta 5 vacio)
--    λ> ej2 = inserta 9 (inserta 4 (inserta 3 vacio))
--    λ> productoC ej1 ej2
--    {(2,3), (2,4), (2,9), (5,3), (5,4), (5,9)}
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TAD_Producto_cartesiano where

import TAD.Conjunto (Conj, vacio, inserta, esVacio, menor, elimina)
import TAD_Transformaciones_conjuntos_listas (conjuntoAlista, listaAconjunto)
import TAD_Union_de_dos_conjuntos (union)
import Test.QuickCheck

-- 1ª solución
-- ===========

productoC :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
productoC c1 c2
  | esVacio c1 = vacio
  | otherwise  = agrega mc1 c2 `union` productoC rc1 c2
  where mc1 = menor c1
        rc1 = elimina mc1 c1

agrega :: (Ord a, Ord b) => a -> Conj b -> Conj (a,b)
agrega x c
  | esVacio c = vacio
  | otherwise = inserta (x, mc) (agrega x rc)
  where mc = menor c
        rc = elimina mc c

-- 2ª solución
-- ===========

productoC2 :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
productoC2 c1 c2 =
  foldr inserta vacio [(x,y) | x <- xs, y <- ys]
  where xs = conjuntoAlista c1
        ys = conjuntoAlista c2

-- 3ª solución
-- ===========

productoC3 :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
productoC3 c1 c2 =
  listaAconjunto [(x,y) | x <- xs, y <- ys]
  where xs = conjuntoAlista c1
        ys = conjuntoAlista c2

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_productoC :: Conj Int -> Conj Int -> Bool
prop_productoC c1 c2 =
  all (== productoC c1 c2)
      [productoC2 c1 c2,
       productoC3 c1 c2]

-- La comprobación es
--    λ> quickCheck prop_productoC
--    +++ OK, passed 100 tests.
