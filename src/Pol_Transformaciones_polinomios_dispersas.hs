-- Pol_Transformaciones_polinomios_dispersas.hs
-- TAD de los polinomios: Transformaciones entre polinomios y listas dispersas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-abril-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los polinomios](https://bit.ly/3KwqXYu)
-- definir las funciones
--    dispersaApolinomio :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
--    polinomioAdispersa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
-- tales que
-- + (dispersaApolinomio ps) es el polinomiocuya representación dispersa
--   es ps. Por ejemplo,
--      λ> dispersaApolinomio [(6,9),(3,5),(1,4),(0,7)]
--      9*x^6 + 5*x^3 + 4*x + 7
-- + (polinomioAdispersa p) es la representación dispersa del polinomio
--   p. Por ejemplo,
--      λ> ejPol = consPol 6 9 (consPol 3 5 (consPol 1 4 (consPol 0 7 polCero)))
--      λ> ejPol
--      9*x^6 + 5*x^3 + 4*x + 7
--      λ> polinomioAdispersa ejPol
--      [(6,9),(3,5),(1,4),(0,7)]
--
-- Comprobar con QuickCheck que ambas funciones son inversas.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Pol_Transformaciones_polinomios_dispersas where

import TAD.Polinomio (Polinomio, polCero, esPolCero, consPol, grado,
                      coefLider, restoPol)
import Data.List (sort, nub)
import Test.QuickCheck

-- 1ª definición de dispersaApolinomio
-- ===================================

dispersaApolinomio :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio []         = polCero
dispersaApolinomio ((n,a):ps) = consPol n a (dispersaApolinomio ps)

-- 2ª definición de dispersaApolinomio
-- ===================================

dispersaApolinomio2 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio2 = foldr (\(x,y) -> consPol x y) polCero


-- 3ª definición de dispersaApolinomio
-- ===================================

dispersaApolinomio3 :: (Num a, Eq a) => [(Int,a)] -> Polinomio a
dispersaApolinomio3 = foldr (uncurry consPol) polCero

-- Comprobación de equivalencia
-- ============================

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
prop_dispersaApolinomio :: Dispersa -> Bool
prop_dispersaApolinomio (Dis ps) =
  all (== dispersaApolinomio ps)
      [dispersaApolinomio2 ps,
       dispersaApolinomio3 ps]

-- Definición de polinomioAdispersa
-- ================================

polinomioAdispersa :: (Num a, Eq a) => Polinomio a -> [(Int,a)]
polinomioAdispersa p
  | esPolCero p = []
  | otherwise   = (grado p, coefLider p) : polinomioAdispersa (restoPol p)

-- Propiedad de ser inversas
-- =========================

-- La primera propiedad es
prop_polinomioAdispersa_dispersaApolinomio :: Dispersa -> Bool
prop_polinomioAdispersa_dispersaApolinomio (Dis ps) =
  polinomioAdispersa (dispersaApolinomio ps) == ps

-- La comprobación es
--    λ> quickCheck prop_polinomioAdispersa_dispersaApolinomio
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_dispersaApolinomio_polinomioAdispersa :: Polinomio Int -> Bool
prop_dispersaApolinomio_polinomioAdispersa p =
  dispersaApolinomio (polinomioAdispersa p) == p

-- La comprobación es
--    λ> quickCheck prop_dispersaApolinomio_polinomioAdispersa
--    +++ OK, passed 100 tests.
