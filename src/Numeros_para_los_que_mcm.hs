-- Numeros_para_los_que_mcm.hs
-- Números para los que mcm(1,2,...n-1) = mcm(1,2,...,n).
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-junio-2022
-- ---------------------------------------------------------------------

-- --------------------------------------------------------------------- 
-- Un número n es especial si mcm(1,2,...,n-1) = mcm(1,2,...,n). Por
-- ejemplo, el 6 es especial ya que
--    mcm(1,2,3,4,5) = 60 = mcm(1,2,3,4,5,6)
--
-- Definir la sucesión
--    especiales :: [Integer]
-- cuyos términos son los números especiales. Por ejemplo,
--    take 10 especiales     ==  [1,6,10,12,14,15,18,20,21,22]
--    especiales !! 50       ==  84
--    especiales !! 500      ==  638
--    especiales !! 5000     ==  5806
--    especiales !! 50000    ==  55746
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_para_los_que_mcm where

import Test.QuickCheck (NonNegative(NonNegative), quickCheck)

-- 1ª solución
-- ===========

especiales1 :: [Integer]
especiales1 = filter especial1 [1..]

especial1 :: Integer -> Bool
especial1 n = mcm1 [1..n-1] == mcm1 [1..n]

mcm1 :: [Integer] -> Integer
mcm1 []     = 1
mcm1 (x:xs) = lcm x (mcm1 xs)

-- 2ª solución
-- ===========

especiales2 :: [Integer]
especiales2 = filter especial2 [1..]

especial2 :: Integer -> Bool
especial2 n = mcm2 [1..n-1] == mcm2 [1..n]

mcm2 :: [Integer] -> Integer
mcm2 = foldr lcm 1

-- 3ª solución
-- ===========

especiales3 :: [Integer]
especiales3 = [n | ((n,x),(_,y)) <- zip mcms (tail mcms)
                 , x == y]

mcms :: [(Integer,Integer)]
mcms = zip [1..] (scanl lcm 1 [1..])

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_especiales :: NonNegative Int -> Bool
prop_especiales (NonNegative n) =
  all (== especiales1 !! n)
      [especiales2 !! n,
       especiales3 !! n]

-- La comprobación es
--    λ> quickCheck prop_especiales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es

-- Comparación 
--    λ> especiales1 !! 2000
--    2390
--    (3.38 secs, 4,724,497,192 bytes)
--    λ> especiales2 !! 2000
--    2390
--    (1.91 secs, 4,303,415,512 bytes)
--    λ> especiales3 !! 2000
--    2390
--    (0.01 secs, 4,209,664 bytes)

