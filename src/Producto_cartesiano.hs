-- Producto_cartesiano.hs
-- Producto cartesiano de una familia de conjuntos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos xss.
-- Por ejemplo,
--    λ> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    λ> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    λ> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    λ> producto []
--    [[]]
--
-- Comprobar con QuickCheck que para toda lista de listas de números
-- enteros, xss, se verifica que el número de elementos de (producto
-- xss) es igual al producto de los números de elementos de cada una de
-- las listas de xss.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Producto_cartesiano where

import Test.QuickCheck (quickCheck)
import Control.Monad (liftM2)
import Control.Applicative (liftA2)

-- 1ª solución
-- ===========

producto1 :: [[a]] -> [[a]]
producto1 []       = [[]]
producto1 (xs:xss) = [x:ys | x <- xs, ys <- producto1 xss]

-- 2ª solución
-- ===========

producto2 :: [[a]] -> [[a]]
producto2 []       = [[]]
producto2 (xs:xss) = [x:ys | x <- xs, ys <- ps]
  where ps = producto2 xss

-- 3ª solución
-- ===========

producto3 :: [[a]] -> [[a]]
producto3 []       = [[]]
producto3 (xs:xss) = inserta3 xs (producto3 xss)

-- (inserta xs xss) inserta cada elemento de xs en los elementos de
-- xss. Por ejemplo,
--    λ> inserta [1,2] [[3,4],[5,6]]
--    [[1,3,4],[1,5,6],[2,3,4],[2,5,6]]
inserta3 :: [a] -> [[a]] -> [[a]]
inserta3 [] _       = []
inserta3 (x:xs) yss = [x:ys | ys <- yss] ++ inserta3 xs yss

-- 4ª solución
-- ===========

producto4 :: [[a]] -> [[a]]
producto4 []       = [[]]
producto4 (xs:xss) = inserta4 xs (producto4 xss)

inserta4 :: [a] -> [[a]] -> [[a]]
inserta4 [] _       = []
inserta4 (x:xs) yss = map (x:) yss ++ inserta4 xs yss

-- 5ª solución
-- ===========

producto5 :: [[a]] -> [[a]]
producto5 []       = [[]]
producto5 (xs:xss) = inserta5 xs (producto5 xss)

inserta5 :: [a] -> [[a]] -> [[a]]
inserta5 xs xss = [x:ys | x <- xs, ys <- xss]

-- 6ª solución
-- ===========

producto6 :: [[a]] -> [[a]]
producto6 []       = [[]]
producto6 (xs:xss) = inserta6 xs (producto6 xss)

inserta6 :: [a] -> [[a]] -> [[a]]
inserta6 xs xss = concatMap (\x -> map (x:) xss) xs

-- 7ª solución
-- ===========

producto7 :: [[a]] -> [[a]]
producto7 []       = [[]]
producto7 (xs:xss) = inserta7 xs (producto7 xss)

inserta7 :: [a] -> [[a]] -> [[a]]
inserta7 xs xss = xs >>= (\x -> map (x:) xss)

-- 8ª solución
-- ===========

producto8 :: [[a]] -> [[a]]
producto8 []       = [[]]
producto8 (xs:xss) = inserta8 xs (producto8 xss)

inserta8 :: [a] -> [[a]] -> [[a]]
inserta8 xs xss = (:) <$> xs <*> xss

-- 9ª solución
-- ===========

producto9 :: [[a]] -> [[a]]
producto9 []       = [[]]
producto9 (xs:xss) = inserta9 xs (producto9 xss)

inserta9 :: [a] -> [[a]] -> [[a]]
inserta9 = liftA2 (:)

-- 10ª solución
-- ===========

producto10 :: [[a]] -> [[a]]
producto10 []       = [[]]
producto10 (xs:xss) = inserta10 xs (producto10 xss)

inserta10 :: [a] -> [[a]] -> [[a]]
inserta10 = liftM2 (:)

-- 11ª solución
-- ===========

producto11 :: [[a]] -> [[a]]
producto11 = foldr (liftM2 (:)) [[]]

-- 12ª solución
-- ===========

producto12 :: [[a]] -> [[a]]
producto12 = sequence

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_producto :: [[Int]] -> Bool
prop_producto xss =
  all (== producto1 xss)
      [producto2 xss,
       producto3 xss,
       producto4 xss,
       producto5 xss,
       producto6 xss,
       producto7 xss,
       producto8 xss,
       producto9 xss,
       producto10 xss,
       producto11 xss,
       producto12 xss]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=9}) prop_producto
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (producto1 (replicate 7 [0..9]))
--    10000000
--    (7.82 secs, 10,169,405,464 bytes)
--    λ> length (producto2 (replicate 7 [0..9]))
--    10000000
--    (1.68 secs, 1,333,857,848 bytes)
--    λ> length (producto3 (replicate 7 [0..9]))
--    10000000
--    (2.50 secs, 1,956,089,192 bytes)
--    λ> length (producto4 (replicate 7 [0..9]))
--    10000000
--    (0.81 secs, 1,600,530,312 bytes)
--    λ> length (producto5 (replicate 7 [0..9]))
--    10000000
--    (1.65 secs, 1,333,857,848 bytes)
--    λ> length (producto6 (replicate 7 [0..9]))
--    10000000
--    (0.76 secs, 1,600,522,528 bytes)
--    λ> length (producto7 (replicate 7 [0..9]))
--    10000000
--    (0.27 secs, 1,600,522,248 bytes)
--    λ> length (producto8 (replicate 7 [0..9]))
--    10000000
--    (0.79 secs, 978,305,744 bytes)
--    λ> length (producto9 (replicate 7 [0..9]))
--    10000000
--    (0.77 secs, 1,067,188,576 bytes)
--    λ> length (producto10 (replicate 7 [0..9]))
--    10000000
--    (0.43 secs, 2,311,632,992 bytes)
--    λ> length (producto11 (replicate 7 [0..9]))
--    10000000
--    (0.44 secs, 2,311,632,528 bytes)
--    λ> length (producto12 (replicate 7 [0..9]))
--    10000000
--    (0.77 secs, 1,067,188,128 bytes)
--
--    λ> length (producto4 (replicate 7 [0..10]))
--    19487171
--    (1.47 secs, 3,087,299,560 bytes)
--    λ> length (producto5 (replicate 7 [0..10]))
--    19487171
--    (3.16 secs, 2,572,831,832 bytes)
--    λ> length (producto6 (replicate 7 [0..10]))
--    19487171
--    (1.51 secs, 3,087,290,936 bytes)
--    λ> length (producto7 (replicate 7 [0..10]))
--    19487171
--    (0.53 secs, 3,087,290,656 bytes)
--    λ> length (producto8 (replicate 7 [0..10]))
--    19487171
--    (1.47 secs, 1,886,887,200 bytes)
--    λ> length (producto9 (replicate 7 [0..10]))
--    19487171
--    (2.64 secs, 2,058,367,688 bytes)
--    λ> length (producto10 (replicate 7 [0..10]))
--    19487171
--    (0.77 secs, 4,459,187,088 bytes)
--    λ> length (producto11 (replicate 7 [0..10]))
--    19487171
--    (0.82 secs, 4,459,186,664 bytes)
--    λ> length (producto12 (replicate 7 [0..10]))
--    19487171
--    (2.62 secs, 2,058,367,240 bytes)
--
--    λ> length (producto7 (replicate 7 [1..14]))
--    105413504
--    (2.48 secs, 16,347,726,936 bytes)
--    λ> length (producto10 (replicate 7 [1..14]))
--    105413504
--    (3.74 secs, 23,613,149,576 bytes)
--    λ> length (producto11 (replicate 7 [1..14]))
--    105413504
--    (3.76 secs, 23,613,149,152 bytes)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_length_producto :: [[Int]] -> Bool
prop_length_producto xss =
  length (producto7 xss) == product (map length xss)

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=9}) prop_length_producto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

-- verificacion :: IO ()
-- verificacion = hspec especificacion
--
-- especificacion :: Spec
-- especificacion =
--   describe "producto" $ do
--     it "e1" $
--       producto [[1,3],[2,5]]
--       `shouldBe `[[1,2],[1,5],[3,2],[3,5]]
--     it "e2" $
--       producto [[1,3],[2,5],[6,4]]
--       `shouldBe` [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--     it "e3" $
--       producto [[1,3,5],[2,4]]
--       `shouldBe` [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--     it "e4" $
--       producto ([]::[[Int]])
--       `shouldBe` [[]]
