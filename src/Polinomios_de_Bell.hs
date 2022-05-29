-- Polinomios_de_Bell.hs
-- Polinomios de Bell
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los polinomios de Bell forman una sucesión de polinomios, definida
-- como sigue: 
--    B₀(x) = 1 (polinomio unidad)
--    Bₙ(x) = x·[Bₙ(x) + Bₙ'(x)] (donde Bₙ'(x) es la derivada de Bₙ(x))
-- Por ejemplo,  
--    B₀(x) = 1                     = 1
--    B₁(x) = x·(1+0)               = x     
--    B₂(x) = x·(x+1)               = x²+x         
--    B₃(x) = x·(x²+x+2x+1)         = x³+3x²+x    
--    B₄(x) = x·(x³+3x²+x+3x²+6x+1) = x⁴+6x³+7x²+x       
--
-- Definir la función 
--    polBell :: Integer -> Polinomio Integer
-- tal que (polBell n) es el polinomio de Bell de grado n. Por ejemplo, 
--    polBell 4                    ==  x^4 + 6*x^3 + 7*x^2 + 1*x
--    coeficiente 2 (polBell 4)    ==  7
--    coeficiente 2 (polBell 30)   ==  536870911
--    coeficiente 1 (polBell 1000) == 1
--    length (show (coeficiente 9 (polBell 2000)))  ==  1903
-- 
-- Notas: Se usa la librería I1M.PolOperaciones que se encuentra 
-- [aquí](http://bit.ly/1AKmUQB) y se describe [aquí](http://bit.ly/1NZ0NKo). 
-- Además, en el último ejemplo se usa la función coeficiente tal que
-- (coeficiente k p) es el coeficiente del término de grado k en el
-- polinomio p definida por 
--    coeficiente :: Num a => Int -> Polinomio a -> a
--    coeficiente k p | k == n                 = coefLider p
--                    | k > grado (restoPol p) = 0
--                    | otherwise              = coeficiente k (restoPol p)
--                    where n = grado p
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Polinomios_de_Bell where

import Data.List          (genericIndex)
import I1M.PolOperaciones (Polinomio, coefLider, consPol, derivada,
                           grado, multPol, polCero, polUnidad, restoPol,
                           sumaPol) 
import Test.QuickCheck    (Positive (Positive), quickCheck)

-- Función auxiliar
-- ================

-- (coeficiente k p) es el coeficiente del término de grado k en el
-- polinomio p.
coeficiente :: Num a => Int -> Polinomio a -> a
coeficiente k p | k == n                 = coefLider p
                | k > grado (restoPol p) = 0
                | otherwise              = coeficiente k (restoPol p)
                where n = grado p

-- 1ª solución
-- ===========

polBell1 :: Integer -> Polinomio Integer
polBell1 0 = polUnidad
polBell1 n = multPol (consPol 1 1 polCero) (sumaPol p (derivada p))
  where p = polBell1 (n-1)

-- 2ª solución
-- ===========

polBell2 :: Integer -> Polinomio Integer
polBell2 n = sucPolinomiosBell `genericIndex` n

sucPolinomiosBell :: [Polinomio Integer]
sucPolinomiosBell = iterate f polUnidad
  where f p = multPol (consPol 1 1 polCero) (sumaPol p (derivada p))

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_polBell :: Positive Integer -> Bool 
prop_polBell (Positive n) =
  polBell1 n == polBell2 n

-- La comprobación es
--    λ> quickCheck prop_polBell
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (show (coeficiente 9 (polBell1 2000)))
--    1903
--    (5.37 secs, 4,829,322,368 bytes)
--    λ> length (show (coeficiente 9 (polBell2 2000)))
--    1903
--    (4.03 secs, 4,825,094,064 bytes)
