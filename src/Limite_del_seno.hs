-- Limite_del_seno.hs
-- Aproximación al límite de sen(x)/x cuando x tiende a cero
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El limite de sen(x)/x, cuando x tiende a cero, se puede calcular como
-- el límite de la sucesión sen(1/n)/(1/n), cuando n tiende a infinito.
--
-- Definir las funciones
--    aproxLimSeno :: Int -> [Double]
--    errorLimSeno :: Double -> Int
-- tales que
-- + (aproxLimSeno n) es la lista cuyos elementos son los n primeros
--   términos de la sucesión sen(1/m)/(1/m). Por ejemplo,
--      aproxLimSeno 1 == [0.8414709848078965]
--      aproxLimSeno 2 == [0.8414709848078965,0.958851077208406]
-- + (errorLimSeno x) es el menor número de términos de la sucesión
--   sen(1/m)/(1/m) necesarios para obtener su límite con un error menor
--   que x. Por ejemplo,
--      errorLimSeno 0.1     ==   2
--      errorLimSeno 0.01    ==   5
--      errorLimSeno 0.001   ==  13
--      errorLimSeno 0.0001  ==  41
-- ---------------------------------------------------------------------

module Limite_del_seno where

import Test.QuickCheck

-- 1ª definición de aproxLimSeno
-- =============================

aproxLimSeno1 :: Int -> [Double]
aproxLimSeno1 k = [sin(1/n)/(1/n) | n <- [1..k']]
  where k' = fromIntegral k

-- 2ª definición de aproxLimSeno
-- =============================

aproxLimSeno2 :: Int -> [Double]
aproxLimSeno2 0 = []
aproxLimSeno2 n = aproxLimSeno2 (n-1) ++ [sin(1/n')/(1/n')]
  where n' = fromIntegral n

-- 3ª definición de aproxLimSeno
-- =============================

aproxLimSeno3 :: Int -> [Double]
aproxLimSeno3 = reverse . aux . fromIntegral
  where aux 0 = []
        aux n = sin(1/n)/(1/n) : aux (n-1)

-- 4ª definición de aproxLimSeno
-- =============================

aproxLimSeno4 :: Int -> [Double]
aproxLimSeno4 k = aux [] (fromIntegral k)
  where aux xs 0 = xs
        aux xs n = aux (sin(1/n)/(1/n) : xs) (n-1)

-- 5ª definición de aproxLimSeno
-- =============================

aproxLimSeno5 :: Int -> [Double]
aproxLimSeno5 k =  map (\ n -> sin(1/n)/(1/n)) [1..k']
  where k' = fromIntegral k

-- Comprobación de equivalencia de aproxLimSeno
-- ============================================

-- La propiedad es
prop_aproxLimSeno :: Positive Int -> Bool
prop_aproxLimSeno (Positive k) =
  all (== aproxLimSeno1 k)
      [aproxLimSeno2 k,
       aproxLimSeno3 k,
       aproxLimSeno4 k,
       aproxLimSeno5 k]

-- La comprobación es
--    λ> quickCheck prop_aproxLimSeno
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de aproxLimSeno
-- =========================================

-- La comparación es
--    λ> last (aproxLimSeno1 (2*10^4))
--    0.9999999995833334
--    (0.01 secs, 5,415,816 bytes)
--    λ> last (aproxLimSeno2 (2*10^4))
--    0.9999999995833334
--    (4.48 secs, 17,514,768,064 bytes)
--    λ> last (aproxLimSeno3 (2*10^4))
--    0.9999999995833334
--    (0.02 secs, 9,530,120 bytes)
--    λ> last (aproxLimSeno4 (2*10^4))
--    0.9999999995833334
--    (0.02 secs, 9,529,968 bytes)
--    λ> last (aproxLimSeno5 (2*10^4))
--    0.9999999995833334
--    (0.01 secs, 4,889,720 bytes)
--
--    λ> last (aproxLimSeno1 (2*10^6))
--    0.9999999999999583
--    (0.46 secs, 480,569,808 bytes)
--    λ> last (aproxLimSeno3 (2*10^6))
--    0.9999999999999583
--    (1.96 secs, 896,569,992 bytes)
--    λ> last (aproxLimSeno4 (2*10^6))
--    0.9999999999999583
--    (1.93 secs, 896,570,048 bytes)
--    λ> last (aproxLimSeno5 (2*10^6))
--    0.9999999999999583
--    (0.05 secs, 432,569,800 bytes)
--
--    λ> last (aproxLimSeno1 (10^7))
--    0.9999999999999983
--    (2.26 secs, 2,400,569,760 bytes)
--    λ> last (aproxLimSeno5 (10^7))
--    0.9999999999999983
--    (0.24 secs, 2,160,569,752 bytes)

-- 1ª definición de errorLimSeno
-- =============================

errorLimSeno1 :: Double -> Int
errorLimSeno1 x =
  round (head [m | m <- [1..], abs (1 - sin(1/m)/(1/m)) < x])

-- 2ª definición de errorLimSeno
-- =============================

errorLimSeno2 :: Double -> Int
errorLimSeno2 x = aux 1
  where aux n | abs (1 - sin(1/n)/(1/n)) < x = round n
              | otherwise                    = aux (n+1)

-- 3ª definición de errorLimSeno
-- =============================

errorLimSeno3 :: Double -> Int
errorLimSeno3 x =
  round (head (dropWhile (\ n -> abs (1 - sin(1/n)/(1/n)) >= x) [1..]))

-- Comprobación de equivalencia de errorLimSeno
-- ============================================

-- La propiedad es
prop_errorLimSeno :: Positive Double -> Bool
prop_errorLimSeno (Positive x) =
  all (== errorLimSeno1 x)
      [errorLimSeno2 x,
       errorLimSeno3 x]

-- La comprobación es
--    λ> quickCheck prop_errorLimSeno
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de errorLimSeno
-- =========================================

-- La comparación es
--    λ> errorLimSeno1 (10**(-12))
--    408230
--    (0.41 secs, 206,300,808 bytes)
--    λ> errorLimSeno2 (10**(-12))
--    408230
--    (0.46 secs, 225,895,672 bytes)
--    λ> errorLimSeno3 (10**(-12))
--    408230
--    (0.37 secs, 186,705,688 bytes)
