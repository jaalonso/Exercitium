-- Exponente_mayor.hs
-- Exponente_de la mayor potencia de x que divide a y.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mayorExponente :: Integer -> Integer -> Integer
-- tal que (mayorExponente a b) es el exponente de la mayor potencia de
-- a que divide b. Por ejemplo,
--    mayorExponente 2 8    ==  3
--    mayorExponente 2 9    ==  0
--    mayorExponente 5 100  ==  2
--    mayorExponente 2 60   ==  2
--
-- Nota: Se supone que a > 1 y b > 0.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Exponente_mayor where

import Test.QuickCheck

-- 1ª solución
-- ===========

mayorExponente1 :: Integer -> Integer -> Integer
mayorExponente1 a b
  | rem b a /= 0 = 0
  | otherwise    = 1 + mayorExponente1 a (b `div` a)

-- 2ª solución
-- ===========

mayorExponente2 :: Integer -> Integer -> Integer
mayorExponente2 a b = aux b 0
  where
    aux c r | rem c a /= 0 = r
            | otherwise    = aux (c `div` a) (r + 1)

-- 3ª solución
-- ===========

mayorExponente3 :: Integer -> Integer -> Integer
mayorExponente3 a b = head [x-1 | x <- [0..], mod b (a^x) /= 0]

-- 4ª solución
-- ===========

mayorExponente4 :: Integer -> Integer -> Integer
mayorExponente4 a b =
  fst (until (\ (_,c) -> rem c a /= 0)
             (\ (r,c) -> (r+1, c `div` a))
             (0,b))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_mayorExponente :: Integer -> Integer -> Property
prop_mayorExponente a b =
  a > 1 && b > 0 ==>
  all (== mayorExponente1 a b)
      [mayorExponente2 a b,
       mayorExponente3 a b,
       mayorExponente4 a b]

-- La comprobación es
--    λ> quickCheck prop_mayorExponente
--    +++ OK, passed 100 tests; 457 discarded.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mayorExponente1 2 (2^(5*10^4))
--    50000
--    (0.12 secs, 179,578,424 bytes)
--    λ> mayorExponente2 2 (2^(5*10^4))
--    50000
--    (0.13 secs, 181,533,376 bytes)
--    λ> mayorExponente3 2 (2^(5*10^4))
--    50000
--    (3.88 secs, 818,319,096 bytes)
--    λ> mayorExponente4 2 (2^(5*10^4))
--    50000
--    (0.13 secs, 181,133,344 bytes)
--
--    λ> mayorExponente1 2 (2^(3*10^5))
--    300000
--    (2.94 secs, 5,762,199,064 bytes)
--    λ> mayorExponente2 2 (2^(3*10^5))
--    300000
--    (2.91 secs, 5,773,829,624 bytes)
--    λ> mayorExponente4 2 (2^(3*10^5))
--    300000
--    (3.70 secs, 5,771,396,824 bytes)
