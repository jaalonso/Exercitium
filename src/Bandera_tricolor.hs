-- Bandera_tricolor.hs
-- La bandera tricolor.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-febrero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema de la bandera tricolor consiste en lo siguiente: Dada un
-- lista de objetos xs que pueden ser rojos, amarillos o morados, se pide
-- devolver una lista ys que contiene los elementos de xs, primero los
-- rojos, luego los amarillos y por último los morados.
--
-- Definir el tipo de dato Color para representar los colores con los
-- constructores R, A y M correspondientes al rojo, azul y morado y la
-- función
--    banderaTricolor :: [Color] -> [Color]
-- tal que (banderaTricolor xs) es la bandera tricolor formada con los
-- elementos de xs. Por ejemplo,
--    banderaTricolor [M,R,A,A,R,R,A,M,M]  ==  [R,R,R,A,A,A,M,M,M]
--    banderaTricolor [M,R,A,R,R,A]        ==  [R,R,R,A,A,M]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Bandera_tricolor where

import Data.List (sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Color = R | A | M
  deriving (Show, Eq, Ord, Enum)

-- 1ª solución
-- ===========

banderaTricolor1 :: [Color] -> [Color]
banderaTricolor1 xs =
  [x | x <- xs, x == R] ++
  [x | x <- xs, x == A] ++
  [x | x <- xs, x == M]

-- 2ª solución
-- ===========

banderaTricolor2 :: [Color] -> [Color]
banderaTricolor2 xs =
  colores R ++ colores A ++ colores M
  where colores c = filter (== c) xs

-- 3ª solución
-- ===========

banderaTricolor3 :: [Color] -> [Color]
banderaTricolor3 xs =
  concat [[x | x <- xs, x == c] | c <- [R,A,M]]

-- 4ª solución
-- ===========

banderaTricolor4 :: [Color] -> [Color]
banderaTricolor4 xs = aux xs ([],[],[])
  where aux []     (rs,as,ms) = rs ++ as ++ ms
        aux (R:ys) (rs,as,ms) = aux ys (R:rs,   as,   ms)
        aux (A:ys) (rs,as,ms) = aux ys (  rs, A:as,   ms)
        aux (M:ys) (rs,as,ms) = aux ys (  rs,   as, M:ms)

-- 5ª solución
-- ===========

banderaTricolor5 :: [Color] -> [Color]
banderaTricolor5 = sort

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Color] -> [Color]) -> Spec
specG banderaTricolor = do
  it "e1" $
    banderaTricolor [M,R,A,A,R,R,A,M,M]  `shouldBe`  [R,R,R,A,A,A,M,M,M]
  it "e2" $
    banderaTricolor [M,R,A,R,R,A]        `shouldBe`  [R,R,R,A,A,M]

spec :: Spec
spec = do
  describe "def. 1" $ specG banderaTricolor1
  describe "def. 2" $ specG banderaTricolor2
  describe "def. 3" $ specG banderaTricolor3
  describe "def. 4" $ specG banderaTricolor4
  describe "def. 5" $ specG banderaTricolor5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

instance Arbitrary Color where
  arbitrary = elements [A,R,M]

-- La propiedad es
prop_banderaTricolor :: [Color] -> Bool
prop_banderaTricolor xs =
  all (== banderaTricolor1 xs)
      [banderaTricolor2 xs,
       banderaTricolor3 xs,
       banderaTricolor4 xs,
       banderaTricolor5 xs]

-- La comprobación es
--    λ> quickCheck prop_banderaTricolor
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> bandera n = concat [replicate n c | c <- [M,R,A]]
--    λ> length (banderaTricolor1 (bandera (10^6)))
--    3000000
--    (1.51 secs, 1,024,454,768 bytes)
--    λ> length (banderaTricolor1 (bandera (2*10^6)))
--    6000000
--    (2.94 secs, 2,048,454,832 bytes)
--    λ> length (banderaTricolor2 (bandera (2*10^6)))
--    6000000
--    (2.35 secs, 1,232,454,920 bytes)
--    λ> length (banderaTricolor3 (bandera (2*10^6)))
--    6000000
--    (4.28 secs, 2,304,455,360 bytes)
--    λ> length (banderaTricolor4 (bandera (2*10^6)))
--    6000000
--    (3.01 secs, 1,904,454,672 bytes)
--    λ> length (banderaTricolor5 (bandera (2*10^6)))
--    6000000
--    (2.47 secs, 1,248,454,744 bytes)
