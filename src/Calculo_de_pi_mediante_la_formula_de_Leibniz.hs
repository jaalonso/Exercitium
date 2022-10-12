-- Calculo_de_pi_mediante_la_formula_de_Leibniz.hs
-- Cálculo del número π mediante la fórmula de Leibniz.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-octubre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El número π puede calcularse con la [fórmula de
-- Leibniz](https://bit.ly/3ERCwZd)
--    π/4 = 1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1) + ...
--
-- Definir las funciones
--    calculaPi :: Int -> Double
--    errorPi   :: Double -> Int
-- tales que
-- + (calculaPi n) es la aproximación del número π calculada
--   mediante la expresión
--      4*(1 - 1/3 + 1/5 - 1/7 + ...+ (-1)**n/(2*n+1))
--   Por ejemplo,
--      calculaPi 3    ==  2.8952380952380956
--      calculaPi 300  ==  3.1449149035588526
-- + (errorPi x) es el menor número de términos de la serie
--    necesarios para obtener pi con un error menor que x. Por ejemplo,
--      errorPi 0.1    ==    9
--      errorPi 0.01   ==   99
--      errorPi 0.001  ==  999
-- ---------------------------------------------------------------------

module Calculo_de_pi_mediante_la_formula_de_Leibniz where

import Test.QuickCheck

-- 1ª definición de calculaPi
-- ==========================

calculaPi1 :: Int -> Double
calculaPi1 k = 4 * sum [(-1)**n/(2*n+1) | n <- [0..k']]
  where k' = fromIntegral k

-- 2ª definición de calculaPi
-- ==========================

calculaPi2 :: Int -> Double
calculaPi2 0 = 4
calculaPi2 n = calculaPi2 (n-1) + 4*(-1)**n'/(2*n'+1)
  where n' = fromIntegral n

-- 3ª definición de calculaPi
-- ==========================

calculaPi3 :: Int -> Double
calculaPi3 = aux . fromIntegral
  where aux 0 = 4
        aux n = 4*(-1)**n/(2*n+1) + aux (n-1)

-- Comprobación de equivalencia de calculaPi
-- =========================================

-- La propiedad es
prop_calculaPi :: Positive Int -> Bool
prop_calculaPi (Positive k) =
  all (== calculaPi1 k)
      [calculaPi2 k,
       calculaPi3 k]

-- La comprobación es
--    λ> quickCheck prop_calculaPi
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de calculaPi
-- ======================================

-- La comparación es
--    λ> calculaPi1 (10^6)
--    3.1415936535887745
--    (1.31 secs, 609,797,408 bytes)
--    λ> calculaPi2 (10^6)
--    3.1415936535887745
--    (1.68 secs, 723,032,272 bytes)
--    λ> calculaPi3 (10^6)
--    3.1415936535887745
--    (2.22 secs, 1,099,032,608 bytes)


-- 1ª definición de errorPi
-- ========================

errorPi1 :: Double -> Int
errorPi1 x =
  head [n | n <- [1..]
          , abs (pi - calculaPi1 n) < x]

-- 2ª definición de errorPi
-- ========================

errorPi2 :: Double -> Int
errorPi2 x = aux 1
  where aux n | abs (pi - calculaPi1 n) < x = n
              | otherwise                   = aux (n+1)

-- Comprobación de equivalencia de errorPi
-- ============================================

-- La propiedad es
prop_errorPi :: Positive Double -> Bool
prop_errorPi (Positive x) =
  errorPi1 x == errorPi2 x

-- La comprobación es
--    λ> quickCheck prop_errorPi
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de errorPi
-- ====================================

-- La comparación es
--    λ> errorPi1 0.0005
--    1999
--    (1.88 secs, 1,189,226,384 bytes)
--    λ> errorPi2 0.0005
--    1999
--    (1.87 secs, 1,213,341,096 bytes)
