-- Pol_Transformaciones_dispersa_y_densa.hs
-- TAD de los polinomios: Transformaciones entre las representaciones dispersa y densa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 18-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir las funciones
--    densaAdispersa :: (Num a, Eq a) => [a] -> [(Int,a)]
--    dispersaAdensa :: (Num a, Eq a) => [(Int,a)] -> [a]
-- tales que
-- + (densaAdispersa xs) es la representación dispersa del polinomio
--   cuya representación densa es xs. Por ejemplo,
--      λ> densaAdispersa [9,0,0,5,0,4,7]
--      [(6,9),(3,5),(1,4),(0,7)]
-- + (dispersaAdensa ps) es la representación densa del polinomio
--   cuya representación dispersa es ps. Por ejemplo,
--      λ> dispersaAdensa [(6,9),(3,5),(1,4),(0,7)]
--      [9,0,0,5,0,4,7]
--
-- Comprobar con QuickCheck que las funciones densaAdispersa y
-- dispersaAdensa son inversas.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Transformaciones_dispersa_y_densa where

import Data.List (nub, sort)
import Test.QuickCheck

-- 1ª definición de densaAdispersa
-- ===============================

densaAdispersa :: (Num a, Eq a) => [a] -> [(Int,a)]
densaAdispersa xs = [(m,a) | (m,a) <- zip [n-1,n-2..] xs, a /= 0]
  where n  = length xs

-- 2ª definición de densaAdispersa
-- ===============================

densaAdispersa2 :: (Num a, Eq a) => [a] -> [(Int,a)]
densaAdispersa2 xs = reverse (aux (reverse xs) 0)
  where aux [] _ = []
        aux (0:ys) n = aux ys (n+1)
        aux (y:ys) n = (n,y) : aux ys (n+1)

-- Comprobación de equivalencia de densaAdispersa
-- ==============================================

-- La propiedad es
prop_densaAdispersa :: [Int] -> Bool
prop_densaAdispersa xs =
  densaAdispersa xs == densaAdispersa2 xs

-- La comprobación es
--    λ> quickCheck prop_densaAdispersa
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> densaAdispersa (5 : replicate (10^7) 0)
--    [(10000000,5)]
--    (4.54 secs, 3,280,572,504 bytes)
--    λ> densaAdispersa2 (5 : replicate (10^7) 0)
--    [(10000000,5)]
--    (7.35 secs, 3,696,968,576 bytes)

-- 1ª definición de dispersaAdensa
-- ===============================

dispersaAdensa :: (Num a, Eq a) => [(Int,a)] -> [a]
dispersaAdensa []      = []
dispersaAdensa [(n,a)] = a : replicate n 0
dispersaAdensa ((n,a):(m,b):ps) =
  a : replicate (n-m-1) 0 ++ dispersaAdensa ((m,b):ps)

-- 2ª definición de dispersaAdensa
-- ===============================

dispersaAdensa2 :: (Num a, Eq a) => [(Int,a)] -> [a]
dispersaAdensa2 []           = []
dispersaAdensa2 ps@((n,_):_) =
  [coeficiente ps m | m <- [n,n-1..0]]

-- (coeficiente ps n) es el coeficiente del término de grado n en el
-- polinomio cuya representación densa es ps. Por ejemplo,
--    coeficiente [(6,9),(3,5),(1,4),(0,7)] 3  ==  5
--    coeficiente [(6,9),(3,5),(1,4),(0,7)] 4  ==  0
coeficiente :: (Num a, Eq a) => [(Int,a)] -> Int -> a
coeficiente [] _                     = 0
coeficiente ((m,a):ps) n | n > m     = 0
                         | n == m    = a
                         | otherwise = coeficiente ps n

-- Comprobación de equivalencia de dispersaAdensa
-- ==============================================

-- Tipo de las representaciones dispersas de polinomios.
newtype Dispersa = Dis [(Int,Int)]
  deriving Show

-- dispersaArbitraria es un generador de representaciones dispersas de
-- polinomios. Por ejemplo,
--    λ> sample dispersaArbitraria
--    Dis []
--    Dis []
--    Dis [(3,-2),(2,0),(0,3)]
--    Dis [(6,1),(4,-2),(3,4),(2,-4)]
--    Dis []
--    Dis [(5,-7)]
--    Dis [(12,5),(11,-8),(10,3),(8,-10),(7,-5),(4,12),(3,6),(2,-8),(1,11)]
--    Dis [(7,-2),(2,-8)]
--    Dis [(14,-15)]
--    Dis [(17,5),(16,1),(15,-1),(14,10),(13,5),(12,-15),(9,12),(6,14)]
--    Dis [(19,17),(12,7),(8,-3),(7,13),(5,-2),(4,7)]
dispersaArbitraria :: Gen Dispersa
dispersaArbitraria = do
  (xs, ys) <- arbitrary
  let xs' = nub (reverse (sort (map abs xs)))
      ys' = filter (/= 0) ys
  return (Dis (zip xs' ys'))

-- Dispersa está contenida en Arbitrary
instance Arbitrary Dispersa where
  arbitrary = dispersaArbitraria

-- La propiedad es
prop_dispersaAdensa :: Dispersa -> Bool
prop_dispersaAdensa (Dis xs) =
  dispersaAdensa xs == dispersaAdensa2 xs

-- La comprobación es
--    λ> quickCheck prop_dispersaAdensa
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de dispersaAdensa
-- ===========================================

-- La comparación es
--    λ> length (dispersaAdensa [(10^7,5)])
--    10000001
--    (0.11 secs, 560,566,848 bytes)
--    λ> length (dispersaAdensa2 [(10^7,5)])
--    10000001
--    (2.51 secs, 2,160,567,112 bytes)

-- Propiedad
-- =========

-- Tipo de las representaciones densas de polinomios.
newtype Densa = Den [Int]
  deriving Show

-- densaArbitraria es un generador de representaciones dispersas de
-- polinomios. Por ejemplo,
--    λ> sample densaArbitraria
--    Den []
--    Den []
--    Den []
--    Den [-6,6,5,-3]
--    Den []
--    Den [8,-7,-10,8,-10,-4,10,6,10]
--    Den [-6,2,11,-4,-9,-5,9,2,2,9]
--    Den [-6,9,-2]
--    Den [-1,-7,15,1,5,-2,13,16,8,7,2,16,-2,16,-7,4]
--    Den [8,13,-4,-2,-10,3,5,-4,-6,13,-9,-12,8,11,9,-18,12,10]
--    Den [-1,-2,11,17,-7,13,-12,-19,16,-10,-18,-19,1,-4,-17,10,1,10]
densaArbitraria :: Gen Densa
densaArbitraria = do
  ys <- arbitrary
  let ys' = dropWhile (== 0) ys
  return (Den ys')

-- Dispersa está contenida en Arbitrary
instance Arbitrary Densa where
  arbitrary = densaArbitraria

-- La primera propiedad es
prop_dispersaAdensa_densaAdispersa :: Densa -> Bool
prop_dispersaAdensa_densaAdispersa (Den xs) =
  dispersaAdensa (densaAdispersa xs) == xs

-- La comprobación es
--    λ> quickCheck prop_dispersaAdensa_densaAdispersa
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_densaAdispersa_dispersaAdensa :: Dispersa -> Bool
prop_densaAdispersa_dispersaAdensa (Dis ps) =
  densaAdispersa (dispersaAdensa ps) == ps

-- La comprobación es
--    λ> quickCheck prop_densaAdispersa_dispersaAdensa
--    +++ OK, passed 100 tests.
