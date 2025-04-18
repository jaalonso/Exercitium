-- Descomposiciones_triangulares.hs
-- Descomposiciones triangulares
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 5-junio-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números triangulares se forman como sigue
--
--    *     *      *
--         * *    * *
--               * * *
--    1     3      6
--
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1 + 2
--     6 = 1 + 2 + 3
--    10 = 1 + 2 + 3 + 4
--    15 = 1 + 2 + 3 + 4 + 5
--
-- Definir la función
--    descomposicionesTriangulares :: Int -> [(Int, Int, Int)]
-- tal que (descomposicionesTriangulares n) es la lista de las
-- ternas correspondientes a las descomposiciones de n en tres sumandos,
-- como máximo, formados por números triangulares. Por ejemplo,
--    λ> descomposicionesTriangulares 4
--    []
--    λ> descomposicionesTriangulares 5
--    [(1,1,3)]
--    λ> descomposicionesTriangulares 12
--    [(1,1,10),(3,3,6)]
--    λ> descomposicionesTriangulares 30
--    [(1,1,28),(3,6,21),(10,10,10)]
--    λ> descomposicionesTriangulares 61
--    [(1,15,45),(3,3,55),(6,10,45),(10,15,36)]
--    λ> descomposicionesTriangulares 52
--    [(1,6,45),(1,15,36),(3,21,28),(6,10,36),(10,21,21)]
--    λ> descomposicionesTriangulares 82
--    [(1,3,78),(1,15,66),(1,36,45),(6,10,66),(6,21,55),(10,36,36)]
--    λ> length (descomposicionesTriangulares (5*10^5))
--    124
-- ---------------------------------------------------------------------

module A2014.M06.Descomposiciones_triangulares where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

descomposicionesTriangulares1 :: Int -> [(Int, Int, Int)]
descomposicionesTriangulares1 n =
  [(x,y,z) | x <- xs,
             y <- xs,
             z <- xs,
             x <= y && y <= z,
             x + y + z == n]
  where xs = takeWhile (<=n) triangulares

-- triangulares es la lista de los números triangulares. Por ejemplo,
--    take 9 triangulares  ==  [1,3,6,10,15,21,28,36,45]
triangulares :: [Int]
triangulares = scanl (+) 1 [2..]

-- 2ª solución
-- ===========

descomposicionesTriangulares2 :: Int -> [(Int, Int, Int)]
descomposicionesTriangulares2 n =
  [(x,y,z) | x <- xs,
             y <- xs,
             x <= y,
             z <- xs,
             y <= z,
             x + y + z == n]
  where xs = takeWhile (<=n) triangulares

-- 3ª solución
-- ===========

descomposicionesTriangulares3 :: Int -> [(Int, Int, Int)]
descomposicionesTriangulares3 n =
  [(x,y,z) | x <- xs,
             y <- xs,
             x <= y,
             let z = n - x - y,
             y <= z,
             z `elem` xs]
  where xs = takeWhile (<=n) triangulares

-- 4ª solución
-- ===========

descomposicionesTriangulares4 :: Int -> [(Int, Int, Int)]
descomposicionesTriangulares4 n =
  [(x,y,n-x-y) | x <- xs,
                 y <- dropWhile (<x) xs,
                 let z = n - x - y,
                 y <= z,
                 z `elem` xs]
  where xs = takeWhile (<=n) triangulares

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [(Int, Int, Int)]) -> Spec
specG descomposicionesTriangulares = do
  it "e1" $
    descomposicionesTriangulares  4 `shouldBe`
      []
  it "e2" $
    descomposicionesTriangulares  5 `shouldBe`
      [(1,1,3)]
  it "e3" $
    descomposicionesTriangulares 12 `shouldBe`
      [(1,1,10),(3,3,6)]
  it "e4" $
    descomposicionesTriangulares 30 `shouldBe`
      [(1,1,28),(3,6,21),(10,10,10)]
  it "e5" $
    descomposicionesTriangulares 61 `shouldBe`
      [(1,15,45),(3,3,55),(6,10,45),(10,15,36)]
  it "e6" $
    descomposicionesTriangulares 52 `shouldBe`
      [(1,6,45),(1,15,36),(3,21,28),(6,10,36),(10,21,21)]
  it "e7" $
    descomposicionesTriangulares 82 `shouldBe`
      [(1,3,78),(1,15,66),(1,36,45),(6,10,66),(6,21,55),(10,36,36)]

spec :: Spec
spec = do
  describe "def. 1" $ specG descomposicionesTriangulares1
  describe "def. 2" $ specG descomposicionesTriangulares2
  describe "def. 3" $ specG descomposicionesTriangulares3
  describe "def. 4" $ specG descomposicionesTriangulares4

-- La verificación es
--    λ> verifica
--    28 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_descomposicionesTriangulares_equiv ::  Positive Int -> Bool
prop_descomposicionesTriangulares_equiv (Positive n) =
  all (== descomposicionesTriangulares1 n)
      [descomposicionesTriangulares2 n,
       descomposicionesTriangulares3 n,
       descomposicionesTriangulares4 n]

-- La comprobación es
--    λ> quickCheck prop_descomposicionesTriangulares_equiv
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> last (descomposicionesTriangulares1 (2*10^4))
--   (5671,6328,8001)
--   (3.34 secs, 1,469,517,168 bytes)
--   λ> last (descomposicionesTriangulares2 (2*10^4))
--   (5671,6328,8001)
--   (1.29 secs, 461,433,928 bytes)
--   λ> last (descomposicionesTriangulares3 (2*10^4))
--   (5671,6328,8001)
--   (0.08 secs, 6,574,056 bytes)
--
--   λ> last (descomposicionesTriangulares3 (5*10^5))
--   (140185,148240,211575)
--   (2.12 secs, 151,137,280 bytes)
--   λ> last (descomposicionesTriangulares4 (5*10^5))
--   (140185,148240,211575)
--   (2.30 secs, 103,280,216 bytes)
