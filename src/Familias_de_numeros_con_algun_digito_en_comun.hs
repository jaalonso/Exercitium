-- Familias_de_numeros_con_algun_digito_en_comun.hs
-- Familias de números con algún dígito en común.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una familia de números es una lista de números tal que todos tienen la
-- misma cantidad de dígitos y, además, dichos números tienen al menos
-- un dígito común.
--
-- Por ejemplo, los números 72, 32, 25 y 22 pertenecen a la misma
-- familia ya que son números de dos dígitos y todos tienen el dígito 2,
-- mientras que los números 123, 245 y 568 no pertenecen a la misma
-- familia, ya que no hay un dígito que aparezca en los tres números.
--
-- Definir la función
--    esFamilia :: [Integer] -> Bool
-- tal que (esFamilia ns) se verifica si ns es una familia de
-- números. Por ejemplo,
--    esFamilia [72, 32, 25, 22]  ==  True
--    esFamilia [123,245,568]     ==  False
--    esFamilia [72, 32, 25, 223] ==  False
--    esFamilia [56]              ==  True
--    esFamilia []                ==  True
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Familias_de_numeros_con_algun_digito_en_comun where

import Data.List (intersect, nub)
import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

esFamilia1 :: [Integer] -> Bool
esFamilia1 [] = True
esFamilia1 ns =
  igualNumeroElementos dss && tieneElementoComun dss
  where dss = map show ns

-- (igualNumeroElementos xss) se verifica si todas las listas de xss
-- tienen el mismo número de elementos. Por ejemplo,
--    igualNumeroElementos [[1,3],[2,2],[4,9]]    ==  True
--    igualNumeroElementos [[1,3],[2,1,2],[4,9]]  ==  False
igualNumeroElementos :: [[a]] -> Bool
igualNumeroElementos xss =
  iguales (map length xss)

-- (iguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    iguales [3,3,3,3]  ==  True
--    iguales [3,3,7,3]  ==  False
iguales :: Eq a => [a] -> Bool
iguales []     = True
iguales (x:xs) = all (==x) xs

-- (tieneElementoComun xss) se verifican si todas las listas de xss
-- tienen algún elemento común. Por ejemplo,
--    tieneElementoComun [[1,2],[2,3],[4,2,7]]  ==  True
--    tieneElementoComun [[1,2],[2,3],[4,3,7]]  ==  False
tieneElementoComun :: Eq a => [[a]] -> Bool
tieneElementoComun []       = False
tieneElementoComun (xs:xss) = any (`esElementoComun` xss) xs

-- (esElementoComun x yss) se verifica si x pertenece a todos los
-- elementos de yss. Por ejemplo,
--    esElementoComun 2 [[1,2],[2,3],[4,2,7]]  ==  True
--    esElementoComun 2 [[1,2],[2,3],[4,3,7]]  ==  False
esElementoComun :: Eq a => a -> [[a]] -> Bool
esElementoComun x = all (x `elem`)

-- 2ª solución
-- ===========

esFamilia2 :: [Integer] -> Bool
esFamilia2 [] = True
esFamilia2 ns =
  igualNumeroElementos2 dss && tieneElementoComun2 dss
  where dss = map show ns

igualNumeroElementos2 :: [[a]] -> Bool
igualNumeroElementos2 xss =
  length (nub (map length xss)) == 1

tieneElementoComun2 :: Eq a => [[a]] -> Bool
tieneElementoComun2 xss =
  not (null (foldl1 intersect xss))

-- 3ª solución
-- ===========

esFamilia3 :: [Integer] -> Bool
esFamilia3 [] = True
esFamilia3 ns =
  igualNumeroElementos3 dss && tieneElementoComun3 dss
  where dss = map show ns

igualNumeroElementos3 :: [[a]] -> Bool
igualNumeroElementos3 = ((==1) . length) . nub . map length

tieneElementoComun3 :: Eq a => [[a]] -> Bool
tieneElementoComun3 = (not . null) . foldl1 intersect

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_esFamilia :: [Integer] -> Bool
prop_esFamilia xss =
  all (== esFamilia1 xss)
      [esFamilia2 xss,
       esFamilia3 xss]

-- La comprobación es
--    λ> quickCheck prop_esFamilia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> esFamilia1 [10^6..4*10^6]
--    False
--    (1.85 secs, 1,931,162,984 bytes)
--    λ> esFamilia2 [10^6..4*10^6]
--    False
--    (2.31 secs, 2,288,177,752 bytes)
--    λ> esFamilia3 [10^6..4*10^6]
--    False
--    (2.23 secs, 2,288,177,864 bytes)
