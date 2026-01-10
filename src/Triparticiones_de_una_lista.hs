-- Triparticiones_de_una_lista.hs
-- Triparticiones de una lista.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-Enero-2015 (actualizado 10-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    triparticiones :: [a] -> [([a],[a],[a])]
-- tal que (triparticiones xs) es la lista de las ternas (xs1,xs2,xs3)
-- tales que su concatenación es xs. Por ejemplo,
--
--    λ> triparticiones "abc"
--    [("","","abc"),("","a","bc"),("","ab","c"),("","abc",""),
--     ("a","","bc"),("a","b","c"),("a","bc",""),("ab","","c"),
--     ("ab","c",""),("abc","","")]
--    λ> length (triparticiones [1..5000])
--    12507501
--
-- Comprobar con QuickCheck que para cada terna de (triparticiones xs)
-- se cumple que la concatenación de sus elementos es xs.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Triparticiones_de_una_lista where

import Data.List (inits, tails)
import Control.Monad (replicateM)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

triparticiones1 :: [a] -> [([a], [a], [a])]
triparticiones1 xs = [(take i xs, take j (drop i xs), drop (i + j) xs)
                     | i <- [0..n],
                       j <- [0..n-i]]
  where n = length xs

-- 2ª solución
-- ===========

triparticiones2 :: [a] -> [([a],[a],[a])]
triparticiones2 xs = [(xs1,xs2,xs3) | (xs1,ys)  <- biparticiones2 xs,
                                      (xs2,xs3) <- biparticiones2 ys]

-- (biparticiones xs) es la lista de los pares (xs1,xs2) tales que su
-- concatenación es xs. Por ejemplo,
--    λ> biparticiones "abc"
--    [("","abc"),("a","bc"),("ab","c"),("abc","")]
biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 []     = [([],[])]
biparticiones2 (x:xs) = ([],x:xs) :
                        [(x:xs1,xs2) | (xs1,xs2) <- biparticiones2 xs]

-- 3ª solución
-- ===========

triparticiones3 :: [a] -> [([a],[a],[a])]
triparticiones3 xs = [(xs1,xs2,xs3) | (xs1,ys)  <- biparticiones3 xs,
                                      (xs2,xs3) <- biparticiones3 ys]

biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 xs = zip (inits xs) (tails xs)

-- 4ª solución
-- ===========

triparticiones4 :: [a] -> [([a], [a], [a])]
triparticiones4 xs = do
  (xs1, ys)  <- biparticiones3 xs
  (xs2, xs3) <- biparticiones3 ys
  return (xs1, xs2, xs3)

-- 5ª solución
-- ===========

triparticiones5 :: [a] -> [([a],[a],[a])]
triparticiones5 xs = [(xs1,xs2,xs3) | (xs1,ys)  <- biparticiones5 xs,
                                      (xs2,xs3) <- biparticiones5 ys]

biparticiones5 :: [a] -> [([a],[a])]
biparticiones5 = zip <$> inits <*> tails

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (String -> [(String,String,String)]) -> Spec
specG triparticiones = do
  it "e1" $
    triparticiones "abc" `shouldBe`
    [("","","abc"),("","a","bc"),("","ab","c"),("","abc",""),
     ("a","","bc"),("a","b","c"),("a","bc",""),("ab","","c"),
     ("ab","c",""),("abc","","")]

spec :: Spec
spec = do
  describe "def. 1" $ specG triparticiones1
  describe "def. 2" $ specG triparticiones2
  describe "def. 3" $ specG triparticiones3
  describe "def. 4" $ specG triparticiones4
  describe "def. 5" $ specG triparticiones5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- -- La propiedad es
prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
  all (== triparticiones1 xs)
      [triparticiones2 xs,
       triparticiones4 xs,
       triparticiones5 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (triparticiones1 [1..500])
--    125751
--    (0.07 secs, 43,020,520 bytes)
--    λ> length (triparticiones2 [1..500])
--    125751
--    (2.55 secs, 3,088,127,152 bytes)
--    λ> length (triparticiones3 [1..500])
--    125751
--    (0.07 secs, 46,082,480 bytes)
--    λ> length (triparticiones4 [1..500])
--    125751
--    (0.06 secs, 65,201,560 bytes)
--    λ> length (triparticiones5 [1..500])
--    125751
--    (0.08 secs, 46,090,624 bytes)
--
--    λ> length (triparticiones1 [1..4000])
--    8006001
--    (1.94 secs, 2,691,961,264 bytes)
--    λ> length (triparticiones3 [1..4000])
--    8006001
--    (1.84 secs, 2,884,457,240 bytes)
--    λ> length (triparticiones4 [1..4000])
--    8006001
--    (1.56 secs, 4,101,401,376 bytes)
--    λ> length (triparticiones5 [1..4000])
--    8006001
--    (1.79 secs, 2,884,521,384 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_triparticiones :: [Int] -> Bool
prop_triparticiones xs =
  all (\(xs1,xs2,xs3) -> xs1 ++ xs2 ++ xs3 == xs) (triparticiones1 xs)

-- La comprobación es
--    λ> quickCheck prop_triparticiones
--    +++ OK, passed 100 tests.
