-- Bandera_tricolor.hs
-- La bandera tricolor.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23-Abril-2014 (Revisión del 22-Agosto-2025)
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

module Bandera_tricolor where

import Data.List (sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Color = R | A | M
  deriving (Show, Eq, Ord, Enum)

-- 1ª solución
-- ===========

banderaTricolor1 :: [Color] -> [Color]
banderaTricolor1 = sort

-- 2ª solución
-- ===========

banderaTricolor2 :: [Color] -> [Color]
banderaTricolor2 xs =
  [x | x <- xs, x == R] ++ [x | x <- xs, x == A] ++ [x | x <- xs, x == M]

-- 3ª solución
-- ===========

banderaTricolor3 :: [Color] -> [Color]
banderaTricolor3 xs =
  concat [[x | x <- xs, x == c] | c <- [R,A,M]]

-- 4ª solución
-- ===========

banderaTricolor4 :: [Color] -> [Color]
banderaTricolor4 xs = aux xs ([],[],[])
  where aux []     (as,rs,ms) = as ++ rs ++ ms
        aux (R:ys) (as,rs,ms) = aux ys (R:as,   rs,   ms)
        aux (A:ys) (as,rs,ms) = aux ys (  as, A:rs,   ms)
        aux (M:ys) (as,rs,ms) = aux ys (  as,   rs, M:ms)

-- 5ª solución
-- ===========

banderaTricolor5 :: [Color] -> [Color]
banderaTricolor5 xs = aux xs (0,0,0)
  where aux []     (as,rs,ms) = replicate as R ++
                                replicate rs A ++
                                replicate ms M
        aux (R:ys) (as,rs,ms) = aux ys (1+as,   rs,   ms)
        aux (A:ys) (as,rs,ms) = aux ys (  as, 1+rs,   ms)
        aux (M:ys) (as,rs,ms) = aux ys (  as,   rs, 1+ms)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Color] -> [Color])  -> Spec
specG banderaTricolor = do
  it "e1" $
    banderaTricolor [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
  it "e2" $
    banderaTricolor [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]

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

-- Equivalencia de las definiciones
-- ================================

instance Arbitrary Color where
  arbitrary = elements [R, A, M]

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
--    λ> :set +s
--    λ> length (banderaTricolor1 (bandera 1000000))
--    3000000
--    (2.92 secs, 1,024,602,024 bytes)
--    λ> length (banderaTricolor2 (bandera 1000000))
--    3000000
--    (1.91 secs, 1,168,601,536 bytes)
--    λ> length (banderaTricolor3 (bandera 1000000))
--    3000000
--    (3.55 secs, 1,440,602,120 bytes)
--    λ> length (banderaTricolor4 (bandera 1000000))
--    3000000
--    (1.30 secs, 1,000,601,376 bytes)
--    λ> length (banderaTricolor5 (bandera 1000000))
--    3000000
--    (1.56 secs, 1,245,461,400 bytes)
