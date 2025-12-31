-- Conjuntos_de_puntos_enteros_en_regiones_rectangulares.hs
-- Conjuntos de puntos enteros en regiones rectangulares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-Enero-2015 (actualizado 31-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los puntos de una cuadrícula se puede representar mediante pares de
-- números enteror
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
--    puntos :: Region -> [Punto]
-- tal que (puntos r) es la lista de puntos de la región r. Por ejemplo,
-- usando las regiones definidas por
--    r0021, r3051, r4162 :: Region
--    r0021 = Rectangulo (0,0) (2,1)
--    r3051 = Rectangulo (3,0) (5,1)
--    r4162 = Rectangulo (4,1) (6,2)
-- se tiene
--    λ> puntos r0021
--    [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
--    λ> puntos r3051
--    [(3,0),(3,1),(4,0),(4,1),(5,0),(5,1)]
--    λ> puntos r4162
--    [(4,1),(4,2),(5,1),(5,2),(6,1),(6,2)]
--    λ> puntos (Union r0021 r3051)
--    [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1),(3,0),(3,1),(4,0),(4,1),(5,0),(5,1)]
--    λ> puntos (Diferencia r3051 r4162)
--    [(3,0),(3,1),(4,0),(5,0)]
--    λ> puntos (Union (Diferencia r3051 r4162) r4162)
--    [(3,0),(3,1),(4,0),(5,0),(4,1),(4,2),(5,1),(5,2),(6,1),(6,2)]
--
--  Usando la función enRegion, que verifica si un punto pertenece a una
--  región, definida en un ejercicio anterior por
--     enRegion :: Punto -> Region -> Bool
--     enRegion (x,y) (Rectangulo (x1,y1) (x2,y2)) =
--       x1 <= x && x <= x2 && y1 <= y && y <= y2
--     enRegion p (Union r1 r2)      = enRegion p r1 || enRegion p r2
--     enRegion p (Diferencia r1 r2) = enRegion p r1 && not (enRegion p r2)
-- comprobar con QuickCheck que (enRegion p r) es equivalente a
-- (p `elem` puntos r).
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Conjuntos_de_puntos_enteros_en_regiones_rectangulares where

import qualified Data.Set as S
import Data.List (nub, union, sort, (\\))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Punto = (Int,Int)

data Region = Rectangulo Punto  Punto
            | Union      Region Region
            | Diferencia Region Region
  deriving (Eq, Show)

r0021, r3051, r4162 :: Region
r0021 = Rectangulo (0,0) (2,1)
r3051 = Rectangulo (3,0) (5,1)
r4162 = Rectangulo (4,1) (6,2)

-- 1ª solución
-- ===========

puntos1 :: Region -> [Punto]
puntos1 (Rectangulo (x1, y1) (x2, y2)) =
  [(x, y) | x <- [x1..x2], y <- [y1..y2]]
puntos1 (Union r1 r2) =
  nub (puntos1 r1 ++ puntos1 r2)
puntos1 (Diferencia r1 r2) =
  [p | p <- puntos1 r1, p `notElem` puntos1 r2]

-- 2ª solución
-- ===========

puntos2 :: Region -> [Punto]
puntos2 (Rectangulo (x1,y1) (x2,y2)) =
  [(x,y) | x <- [x1..x2], y <- [y1..y2]]
puntos2 (Union r1 r2)      = puntos2 r1 `union` puntos2 r2
puntos2 (Diferencia r1 r2) = puntos2 r1 \\ puntos2 r2

-- 3ª solución
-- ===========

puntos3 :: Region -> [Punto]
puntos3 r = S.toList (aux r)
  where
    aux (Rectangulo (x1, y1) (x2, y2)) =
      S.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]]
    aux (Union r1 r2) =
      S.union (aux r1) (aux r2)
    aux (Diferencia r1 r2) =
      S.difference (aux r1) (aux r2)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Region -> [Punto]) -> Spec
specG puntos = do
  it "e1" $
    puntos' r0021 `shouldBe`
    [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
  it "e2" $
    puntos' r3051 `shouldBe`
    [(3,0),(3,1),(4,0),(4,1),(5,0),(5,1)]
  it "e3" $
    puntos' r4162 `shouldBe`
    [(4,1),(4,2),(5,1),(5,2),(6,1),(6,2)]
  it "e4" $
    puntos' (Union r0021 r3051) `shouldBe`
    [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1),(3,0),(3,1),(4,0),(4,1),(5,0),(5,1)]
  it "e5" $
    puntos' (Diferencia r3051 r4162) `shouldBe`
    [(3,0),(3,1),(4,0),(5,0)]
  it "e6" $
    puntos' (Union (Diferencia r3051 r4162) r4162) `shouldBe`
    sort [(3,0),(3,1),(4,0),(5,0),(4,1),(4,2),(5,1),(5,2),(6,1),(6,2)]
  where puntos' = sort. puntos

spec :: Spec
spec = do
  describe "def. 1" $ specG puntos1
  describe "def. 2" $ specG puntos2
  describe "def. 3" $ specG puntos3

-- La verificación es
--    λ> verifica
--    18 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

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
prop_equivalencia :: Region -> Bool
prop_equivalencia r =
  all (== sort (puntos1 r))
      [sort (puntos2 r),
       sort (puntos3 r)]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=10}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

ejRegion :: Int -> Region
ejRegion n = foldl1 Union [Rectangulo (i, 0) (i + 5, 1) | i <- [0..n]]

-- La comparación es
--    λ> :set +s
--    λ> length (puntos1 (ejRegion 700))
--    1412
--    (2.90 secs, 96,433,144 bytes)
--    λ> length (puntos2 (ejRegion 700))
--    1412
--    (0.23 secs, 607,642,872 bytes)
--    λ> length (puntos3 (ejRegion 700))
--    1412
--    (0.03 secs, 5,684,560 bytes)
--
--    λ> length (puntos2 (ejRegion 2000))
--    4012
--    (2.21 secs, 5,362,251,560 bytes)
--    λ> length (puntos3 (ejRegion 2000))
--    4012
--    (0.04 secs, 16,103,704 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_puntos :: Punto -> Region -> Bool
prop_puntos p r =
  enRegion p r == (p `elem` puntos1 r)

enRegion :: Punto -> Region -> Bool
enRegion (x,y) (Rectangulo (x1,y1) (x2,y2)) =
  x1 <= x && x <= x2 && y1 <= y && y <= y2
enRegion p (Union r1 r2)      = enRegion p r1 || enRegion p r2
enRegion p (Diferencia r1 r2) = enRegion p r1 && not (enRegion p r2)

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=50}) prop_puntos
--    +++ OK, passed 100 tests.
