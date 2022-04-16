-- Reiteracion_de_funciones.hs
-- Reiteración de una función.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    reiteracion :: (a -> a) -> Int -> a -> a
-- tal que (reiteracion f n x) es el resultado de aplicar n veces la
-- función f a x. Por ejemplo,
--    reiteracion (+1) 10 5  ==  15
--    reiteracion (+5) 10 0  ==  50
--    reiteracion (*2)  4 1  ==  16
--    reiteracion (5:)  4 [] ==  [5,5,5,5]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Reiteracion_de_funciones where

import Test.QuickCheck (Fun (..), Positive (..), quickCheck)

-- 1ª solución
-- ===========

reiteracion1 :: (a -> a) -> Int -> a -> a
reiteracion1 _ 0 x = x
reiteracion1 f n x = f (reiteracion1 f (n-1) x)

-- 2ª solución
-- ===========

reiteracion2 :: (a -> a) -> Int -> a -> a
reiteracion2 _ 0 = id
reiteracion2 f n = f . reiteracion2 f (n-1)

-- 3ª solución
-- ===========

reiteracion3 :: (a -> a) -> Int -> a -> a
reiteracion3 _ 0 = id
reiteracion3 f n
  | even n    = reiteracion3 (f . f) (n `div` 2)
  | otherwise = f . reiteracion3 (f . f) (n `div` 2)

-- 4ª solución
-- ===========

reiteracion4 :: (a -> a) -> Int -> a -> a
reiteracion4 f n x = reiteraciones f x !! n

reiteraciones :: (a -> a) -> a -> [a]
reiteraciones f x = x : reiteraciones f (f x)

-- 5ª solución
-- ===========

reiteracion5 :: (a -> a) -> Int -> a -> a
reiteracion5 f n x = iterate f x !! n

-- 6ª solución
-- ===========

-- Se puede eliminar los argumentos de la definición anterior como sigue:
--    reiteracion4 f n x = iterate f x !! n
--    reiteracion4 f n x = ((!!) (iterate f x)) n
--    reiteracion4 f n x = (((!!) . (iterate f)) x) n
--    reiteracion4 f n x = ((!!) . (iterate f)) x n
--    reiteracion4 f n x = flip ((!!) . (iterate f)) n x
--    reiteracion4 f = flip ((!!) . (iterate f))
--    reiteracion4 f = flip (((!!) .) (iterate f))
--    reiteracion4 f = flip (((!!) .) . iterate) f
--    reiteracion4 f = (flip . ((!!) .) . iterate) f
--    reiteracion4   = flip . ((!!) .) . iterate

reiteracion6 :: (a -> a) -> Int -> a -> a
reiteracion6 = flip . ((!!) .) . iterate

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_reiteracion :: Fun Int Int -> Positive Int -> Int -> Bool
prop_reiteracion (Fun _ f) (Positive n) x =
  all (== reiteracion1 f n x)
      [reiteracion2 f n x,
       reiteracion3 f n x,
       reiteracion4 f n x,
       reiteracion5 f n x,
       reiteracion6 f n x]

-- La comprobación es
--    λ> quickCheck prop_reiteracion
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> reiteracion1 (+1) (10^7) 0
--    10000000
--    (5.09 secs, 2,505,392,792 bytes)
--    λ> reiteracion2 (+1) (10^7) 0
--    10000000
--    (5.45 secs, 2,896,899,728 bytes)
--    λ> reiteracion3 (+1) (10^7) 0
--    10000000
--    (2.14 secs, 816,909,416 bytes)
--    λ> reiteracion4 (+1) (10^7) 0
--    10000000
--    (4.24 secs, 1,696,899,816 bytes)
--    λ> reiteracion5 (+1) (10^7) 0
--    10000000
--    (2.53 secs, 1,376,899,800 bytes)
--    λ> reiteracion6 (+1) (10^7) 0
--    10000000
--    (2.34 secs, 1,376,899,984 bytes)
