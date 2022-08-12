-- Disyuncion_excluyente.hs
-- Disyunción excluyente.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La disyunción excluyente de dos fórmulas se verifica si una es
-- verdadera y la otra es falsa. Su tabla de verdad es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False
--    True  | False | True
--    False | True  | True
--    False | False | False
--
-- Definir la función
--    xor :: Bool -> Bool -> Bool
-- tal que (xor x y) es la disyunción excluyente de x e y. Por ejemplo,
--    xor True  True  == False
--    xor True  False == True
--    xor False True  == True
--    xor False False == False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Disyuncion_excluyente where

import Test.QuickCheck

-- 1ª solución
xor1 :: Bool -> Bool -> Bool
xor1 True  True  = False
xor1 True  False = True
xor1 False True  = True
xor1 False False = False

-- 2ª solución
xor2 :: Bool -> Bool -> Bool
xor2 True  y = not y
xor2 False y = y

-- 3ª solución:
xor3 :: Bool -> Bool -> Bool
xor3 x y = (x || y) && not (x && y)

-- 4ª solución:
xor4 :: Bool -> Bool -> Bool
xor4 x y = (x && not y) || (y && not x)

-- 5ª solución:
xor5 :: Bool -> Bool -> Bool
xor5 x y = x /= y

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_xor :: Bool -> Bool -> Bool
prop_xor x y =
  all (== xor1 x y)
      [xor2 x y,
       xor3 x y,
       xor4 x y,
       xor5 x y]

-- La comprobación es
--    λ> quickCheck prop_xor
--    +++ OK, passed 100 tests.
