-- Puntos_en_regiones_rectangulares.hs
-- Puntos en regiones rectangulares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los puntos se puede representar mediante pares de números
--    type Punto = (Int,Int)
-- y las regiones rectangulares mediante el siguiente tipo de dato
--    data Region = Rectangulo Punto  Punto
--                | Union      Region Region
--                | Diferencia Region Region
--      deriving (Eq, Show)
-- donde
-- + (Rectangulo p1 p2) es la región formada por un rectángulo cuyo
--   vértice superior izquierdo es p1 y su vértice inferior derecho es p2.
-- + (Union r1 r2) es la región cuyos puntos pertenecen a alguna de las
--   regiones r1 y r2.
-- + (Diferencia r1 r2) es la región cuyos puntos pertenecen a la región
--   r1 pero no pertenecen a la r2.
--
-- Definir la función
--    enRegion :: Punto -> Region -> Bool
-- tal que (enRegion p r) se verifica si el punto p pertenece a la
-- región r. Por ejemplo, usando las regiones definidas por
--    r0021, r3051, r4162 :: Region
--    r0021 = Rectangulo (0,0) (2,1)
--    r3051 = Rectangulo (3,0) (5,1)
--    r4162 = Rectangulo (4,1) (6,2)
-- se tiene
--    enRegion (1,0) r0021                                   ==  True
--    enRegion (3,0) r0021                                   ==  False
--    enRegion (1,1) (Union r0021 r3051)                     ==  True
--    enRegion (4,0) (Union r0021 r3051)                     ==  True
--    enRegion (4,2) (Union r0021 r3051)                     ==  False
--    enRegion (3,1) (Diferencia r3051 r4162)                ==  True
--    enRegion (4,1) (Diferencia r3051 r4162)                ==  False
--    enRegion (4,2) (Diferencia r3051 r4162)                ==  False
--    enRegion (4,2) (Union (Diferencia r3051 r4162) r4162)  ==  True
--
-- Comprobar con QuickCheck que si el punto p está en la región r1,
-- entonces, para cualquier región r2, p está en (Union  r1 r2) y en
-- (Union  r2 r1), pero no está en (Diferencia r2 r1).
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Puntos_en_regiones_rectangulares where

import Test.QuickCheck (Arbitrary, Gen, Property, (==>), arbitrary, oneof,
                        sized, generate, quickCheck, quickCheckWith, stdArgs,
                        Args(maxDiscardRatio))

type Punto = (Int,Int)

data Region = Rectangulo Punto  Punto
            | Union      Region Region
            | Diferencia Region Region
  deriving (Eq, Show)

r0021, r3051, r4162 :: Region
r0021 = Rectangulo (0,0) (2,1)
r3051 = Rectangulo (3,0) (5,1)
r4162 = Rectangulo (4,1) (6,2)

enRegion :: Punto -> Region -> Bool
enRegion (x,y) (Rectangulo (x1,y1) (x2,y2)) =
  x1 <= x && x <= x2 &&
  y1 <= y && y <= y2
enRegion p (Union  r1 r2) =
  enRegion p r1 || enRegion p r2
enRegion p (Diferencia r1 r2) =
  enRegion p r1 && not (enRegion p r2)

-- (regionArbitraria n) es un generador de regiones arbitrarias de orden
-- n. Por ejemplo,
--    λ> generate (regionArbitraria 2)
--    Rectangulo (30,-26) (-2,-8)
--    λ> generate (regionArbitraria 2)
--    Union (Union (Rectangulo (-2,-5) (6,1)) (Rectangulo(3,7) (11,15)))
--          (Diferencia (Rectangulo (9,8) (-2,6)) (Rectangulo (-2,2) (7,8)))
regionArbitraria :: Int -> Gen Region
regionArbitraria 0 =
  Rectangulo <$> arbitrary <*> arbitrary
regionArbitraria n =
  oneof [Rectangulo <$> arbitrary <*> arbitrary,
         Union <$> subregion <*> subregion,
         Diferencia <$> subregion <*> subregion]
  where subregion = regionArbitraria (n `div` 2)

-- Region está contenida en Arbitrary
instance Arbitrary Region where
  arbitrary = sized regionArbitraria

-- La propiedad es
prop_enRegion :: Punto -> Region -> Region -> Property
prop_enRegion p r1 r2 =
  enRegion p r1 ==>
  (enRegion p (Union  r1 r2) &&
   enRegion p (Union  r2 r1) &&
   not (enRegion p (Diferencia r2 r1)))

-- La comprobación es
--    λ> quickCheck prop_enRegion
--    *** Gave up! Passed only 78 tests; 1000 discarded tests.
--
--    λ> quickCheckWith (stdArgs {maxDiscardRatio=20}) prop_enRegion
--    +++ OK, passed 100 tests.
