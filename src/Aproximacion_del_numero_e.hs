-- Aproximacion_del_numero_e.hs
-- Aproximación del número e.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El [número e](https://bit.ly/3y17R7l) se define como el límite de la
-- sucesión (1+1/n)**n; es decir,
--    e = lim (1+1/n)**n
--
-- Definir las funciones
--    aproxE      :: Int -> [Double]
--    errorAproxE :: Double -> Int
-- tales que
-- + (aproxE k) es la lista de los k primeros términos de la sucesión
--   (1+1/n)**m. Por ejemplo,
--      aproxE 4 == [2.0,2.25,2.37037037037037,2.44140625]
--      last (aproxE (7*10^7))  ==  2.7182818287372563
-- + (errorE x) es el menor número de términos de la sucesión
--   (1+1/m)**m necesarios para obtener su límite con un error menor que
--   x. Por ejemplo,
--      errorAproxE 0.1    ==  13
--      errorAproxE 0.01   ==  135
--      errorAproxE 0.001  ==  1359
--
-- Indicación: En Haskell, e se calcula como (exp 1).
-- ---------------------------------------------------------------------

module Aproximacion_del_numero_e where

import Test.QuickCheck

-- 1ª definición de aproxE
-- =======================

aproxE1 :: Int -> [Double]
aproxE1 k = [(1+1/n)**n | n <- [1..k']]
  where k' = fromIntegral k

-- 2ª definición de aproxE
-- =======================

aproxE2 :: Int -> [Double]
aproxE2 0 = []
aproxE2 n = aproxE2 (n-1) ++ [(1+1/n')**n']
  where n' = fromIntegral n

-- 3ª definición de aproxE
-- =======================

aproxE3 :: Int -> [Double]
aproxE3 = reverse . aux . fromIntegral
  where aux 0 = []
        aux n = (1+1/n)**n : aux (n-1)

-- 4ª definición de aproxE
-- =======================

aproxE4 :: Int -> [Double]
aproxE4 k = aux [] (fromIntegral k)
  where aux xs 0 = xs
        aux xs n = aux ((1+1/n)**n : xs) (n-1)

-- 5ª definición de aproxE
-- =======================

aproxE5 :: Int -> [Double]
aproxE5 k = map (\ n -> (1+1/n)**n) [1..k']
  where k' = fromIntegral k

-- Comprobación de equivalencia de aproxE
-- ======================================

-- La propiedad es
prop_aproxE :: Positive Int -> Bool
prop_aproxE (Positive k) =
  all (== aproxE1 k)
      [aproxE2 k,
       aproxE3 k,
       aproxE4 k,
       aproxE5 k]

-- La comprobación es
--    λ> quickCheck prop_aproxE
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de aproxE
-- ===================================

-- La comparación es
--    λ> last (aproxE1 (2*10^4))
--    2.718213874533619
--    (0.04 secs, 5,368,968 bytes)
--    λ> last (aproxE2 (2*10^4))
--    2.718213874533619
--    (5.93 secs, 17,514,767,104 bytes)
--    λ> last (aproxE3 (2*10^4))
--    2.718213874533619
--    (0.05 secs, 9,529,336 bytes)
--    λ> last (aproxE4 (2*10^4))
--    2.718213874533619
--    (0.05 secs, 9,529,184 bytes)
--    λ> last (aproxE5 (2*10^4))
--    2.718213874533619
--    (0.01 secs, 4,888,960 bytes)
--
--    λ> last (aproxE1 (2*10^6))
--    2.7182811492688552
--    (0.54 secs, 480,570,120 bytes)
--    λ> last (aproxE3 (2*10^6))
--    2.7182811492688552
--    (2.07 secs, 896,570,280 bytes)
--    λ> last (aproxE4 (2*10^6))
--    2.7182811492688552
--    (2.18 secs, 896,570,336 bytes)
--    λ> last (aproxE5 (2*10^6))
--    2.7182811492688552
--    (0.09 secs, 432,570,112 bytes)

-- 1ª definición de errorAproxE
-- ============================

errorAproxE1 :: Double -> Int
errorAproxE1 x =
  round (head [n | n <- [1..], abs (exp 1 - (1+1/n)**n) < x])

-- 2ª definición de errorAproxE
-- ============================

errorAproxE2 :: Double -> Int
errorAproxE2 x = aux 1
  where aux n | abs (exp 1 - (1+1/n)**n) < x = round n
              | otherwise                    = aux (n+1)

-- 3ª definición de errorAproxE
-- ============================

errorAproxE3 :: Double -> Int
errorAproxE3 x =
  round (head (dropWhile (\ n -> abs (exp 1 - (1+1/n)**n) >= x) [1..]))

-- Comprobación de equivalencia de errorAproxE
-- ===========================================

-- La propiedad es
prop_errorAproxE :: Positive Double -> Bool
prop_errorAproxE (Positive x) =
  all (== errorAproxE1 x)
      [errorAproxE2 x,
       errorAproxE3 x]

-- La comprobación es
--    λ> quickCheck prop_errorAproxE
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de errorAproxE
-- ========================================

-- La comparación es
--    λ> errorAproxE1 0.000001
--    1358611
--    (1.70 secs, 674,424,552 bytes)
--    λ> errorAproxE2 0.000001
--    1358611
--    (1.79 secs, 739,637,704 bytes)
--    λ> errorAproxE3 0.000001
--    1358611
--    (1.20 secs, 609,211,144 bytes)
