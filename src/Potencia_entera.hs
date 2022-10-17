-- Potencia_entera.hs
-- Potencia entera.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
--    potencia 2 3  ==  8
-- ---------------------------------------------------------------------

module Potencia_entera where

import Data.List (foldl')
import Control.Arrow ((***))
import Test.QuickCheck

-- 1ª solución
-- ===========

potencia1 :: Integer -> Integer -> Integer
potencia1 _ 0 = 1
potencia1 m n = m * potencia1 m (n-1)

-- 2ª solución
-- ===========

potencia2 :: Integer -> Integer -> Integer
potencia2 m = aux
  where aux 0 = 1
        aux n = m * aux (n-1)

-- 3ª solución
-- ===========

potencia3 :: Integer -> Integer -> Integer
potencia3 m = aux 1
  where aux r 0 = r
        aux r n = aux (r*m) (n-1)

-- 4ª solución
-- ===========

potencia4 :: Integer -> Integer -> Integer
potencia4 m = aux 1
  where aux r 0 = r
        aux r n = (aux $! (r*m)) $! (n-1)

-- 5ª solución
-- ===========

potencia5 :: Integer -> Integer -> Integer
potencia5 m n = product [m | _ <- [1..n]]

-- 6ª solución
-- ===========

potencia6 :: Integer -> Integer -> Integer
potencia6 m n = foldl' (*) 1 [m | _ <- [1..n]]

-- 7ª solución
-- ===========

potencia7 :: Integer -> Integer -> Integer
potencia7 m n =
  fst (until (\ (_,k) -> k == n)
             (\ (r,k) -> (r*m, k+1))
             (1,0))

-- 8ª solución
-- ===========

potencia8 :: Integer -> Integer -> Integer
potencia8 m n =
  fst (until ((== n) . snd)
             ((m *) *** (1 +))
             (1,0))

-- 9ª solución
-- ===========

potencia9 :: Integer -> Integer -> Integer
potencia9 m n = m^n

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_potencia :: Integer -> NonNegative Integer -> Bool
prop_potencia m (NonNegative n) =
  all (== potencia1 m n)
      [potencia2 m n,
       potencia3 m n,
       potencia4 m n,
       potencia5 m n,
       potencia6 m n,
       potencia7 m n,
       potencia8 m n,
       potencia9 m n]

-- La comprobación es
--    λ> quickCheck prop_potencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (potencia1 2 (2*10^5)))
--    60206
--    (2.97 secs, 2,602,252,408 bytes)
--    λ> length (show (potencia2 2 (2*10^5)))
--    60206
--    (2.63 secs, 2,624,652,624 bytes)
--    λ> length (show (potencia3 2 (2*10^5)))
--    60206
--    (3.41 secs, 2,619,606,368 bytes)
--    λ> length (show (potencia4 2 (2*10^5)))
--    60206
--    (0.64 secs, 2,636,888,928 bytes)
--    λ> length (show (potencia5 2 (2*10^5)))
--    60206
--    (2.47 secs, 2,597,108,000 bytes)
--    λ> length (show (potencia6 2 (2*10^5)))
--    60206
--    (0.35 secs, 2,582,488,824 bytes)
--    λ> length (show (potencia7 2 (2*10^5)))
--    60206
--    (2.48 secs, 2,616,406,272 bytes)
--    λ> length (show (potencia8 2 (2*10^5)))
--    60206
--    (2.40 secs, 2,608,652,736 bytes)
--    λ> length (show (potencia9 2 (2*10^5)))
--    60206
--    (0.01 secs, 4,212,968 bytes)
--
--    λ> length (show (potencia4 2 (10^6)))
--    301030
--    (10.39 secs, 63,963,999,656 bytes)
--    λ> length (show (potencia6 2 (10^6)))
--    301030
--    (8.90 secs, 63,691,999,552 bytes)
--    λ> length (show (potencia9 2 (10^6)))
--    301030
--    (0.04 secs, 19,362,032 bytes)
