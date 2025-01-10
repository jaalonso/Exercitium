-- Maxima_suma_de_caminos_en_un_triangulo.hs
-- Máxima suma de caminos en un triángulo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-enero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los triángulos se pueden representar mediante listas de listas. Por
-- ejemplo, el triángulo
--       3
--      7 4
--     2 4 6
--    8 5 9 3
-- se representa por
--    [[3],[7,4],[2,4,6],[8,5,9,3]]
--
-- Definir la función
--    maximaSuma :: [[Integer]] -> Integer
-- tal que (maximaSuma xss) es el máximo de las sumas de los elementos
-- de los caminos en el triángulo xss donde los caminos comienzan en el
-- elemento de la primera fila, en cada paso se mueve a uno de  sus dos
-- elementos adyacentes en la fila siguiente y terminan en la última
-- fila. Por ejemplo,
--    maximaSuma [[3],[7,4]]                    ==  10
--    maximaSuma [[3],[7,4],[2,4,6]]            ==  14
--    maximaSuma [[3],[7,4],[2,4,6],[8,5,9,3]]  ==  23
--    maximaSuma [[n..n+n] | n <- [0..100]]     ==  10100
--    maximaSuma [[n..n+n] | n <- [0..1000]]    ==  1001000
--    maximaSuma [[n..n+n] | n <- [0..2000]]    ==  4002000
--    maximaSuma [[n..n+n] | n <- [0..3000]]    ==  9003000
--    maximaSuma [[n..n+n] | n <- [0..4000]]    ==  16004000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Maxima_suma_de_caminos_en_un_triangulo where

import Data.List (tails)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

maximaSuma1 :: [[Integer]] -> Integer
maximaSuma1 xss =
  maximum [sum ys | ys <- caminos xss]

caminos :: [[Integer]] -> [[Integer]]
caminos []    = [[]]
caminos [[x]] = [[x]]
caminos ([x]:[y1,y2]:zs) =
  [x:y1:us | (_:us) <- caminos ([y1] : map init zs)] ++
  [x:y2:vs | (_:vs) <- caminos ([y2] : map tail zs)]

-- 2ª solución
-- ===========

maximaSuma2 :: [[Integer]] -> Integer
maximaSuma2 xss = maximum (map sum (caminos xss))

-- 3ª solución
-- ===========

maximaSuma3 :: [[Integer]] -> Integer
maximaSuma3 = maximum . map sum . caminos

-- 4ª solución
-- ===========

maximaSuma4 :: [[Integer]] -> Integer
maximaSuma4 []    = 0
maximaSuma4 [[x]] = x
maximaSuma4 ([x]:[y1,y2]:zs) =
  x + max (maximaSuma4 ([y1] : map init zs))
          (maximaSuma4 ([y2] : map tail zs))

-- 5ª solución
-- ===========

maximaSuma5 :: [[Integer]] -> Integer
maximaSuma5 xss = head (foldr1 g xss)
  where
    f x y z = x + max y z
    g xs ys = zipWith3 f xs ys (tail ys)

-- 6ª solución
-- ===========

maximaSuma6 :: [[Integer]] -> Integer
maximaSuma6 xss = head (foldr1 aux xss)
  where aux a b = zipWith (+) a (zipWith max b (tail b))

-- 7ª solución
-- ===========

maximaSuma7 :: [[Integer]] -> Integer
maximaSuma7 xss = head (foldr (flip f) (last xss) (init xss))
  where f = zipWith ((+) . maximum . take 2) . tails

-- 8ª solución
-- ===========

maximaSuma8 :: [[Integer]] -> Integer
maximaSuma8 = head . foldr1 aux
  where
    aux [] _              = []
    aux (x:xs) (y0:y1:ys) = x + max y0 y1 : aux xs (y1:ys)

-- Comparación de eficiencia
-- =========================

-- Para la comparaciones se usará la siguiente función que construye un
-- triángulo de la altura dada. Por ejemplo,
--    triangulo 2  ==  [[0],[1,2]]
--    triangulo 3  ==  [[0],[1,2],[2,3,4]]
--    triangulo 4  ==  [[0],[1,2],[2,3,4],[3,4,5,6]]
triangulo :: Integer -> [[Integer]]
triangulo n = [[k..k+k] | k <- [0..n-1]]

-- La comparación es
--    (1.97 secs, 876,483,056 bytes)
--    λ> maximaSuma1 (triangulo 19)
--    342
--    (2.37 secs, 1,833,637,824 bytes)
--    λ> maximaSuma2 (triangulo 19)
--    342
--    (2.55 secs, 1,804,276,472 bytes)
--    λ> maximaSuma3 (triangulo 19)
--    342
--    (2.57 secs, 1,804,275,320 bytes)
--    λ> maximaSuma4 (triangulo 19)
--    342
--    (0.28 secs, 245,469,384 bytes)
--    λ> maximaSuma5 (triangulo 19)
--    342
--    (0.01 secs, 153,272 bytes)
--    λ> maximaSuma6 (triangulo 19)
--    342
--    (0.01 secs, 161,360 bytes)
--    λ> maximaSuma7 (triangulo 19)
--    342
--    (0.01 secs, 187,456 bytes)
--    λ> maximaSuma8 (triangulo 19)
--    342
--    (0.01 secs, 191,160 bytes)
--
--    λ> maximaSuma4 (triangulo 22)
--    462
--    (2.30 secs, 1,963,037,888 bytes)
--    λ> maximaSuma5 (triangulo 22)
--    462
--    (0.00 secs, 173,512 bytes)
--    λ> maximaSuma6 (triangulo 22)
--    462
--    (0.01 secs, 182,904 bytes)
--    λ> maximaSuma7 (triangulo 22)
--    462
--    (0.01 secs, 216,560 bytes)
--    λ> maximaSuma8 (triangulo 22)
--    462
--    (0.01 secs, 224,160 bytes)
--
--    λ> maximaSuma5 (triangulo 3000)
--    8997000
--    (2.25 secs, 2,059,784,792 bytes)
--    λ> maximaSuma6 (triangulo 3000)
--    8997000
--    (2.15 secs, 2,404,239,896 bytes)
--    λ> maximaSuma7 (triangulo 3000)
--    8997000
--    (1.53 secs, 2,612,659,504 bytes)
--    λ> maximaSuma8 (triangulo 3000)
--    8997000
--    (3.47 secs, 3,520,910,256 bytes)
--
--    λ> maximaSuma7 (triangulo 4000)
--    15996000
--    (3.12 secs, 4,634,841,200 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Integer]] -> Integer) -> Spec
specG maximaSuma = do
  it "e1" $
    maximaSuma [[3],[7,4]]                    `shouldBe`  10
  it "e2" $
    maximaSuma [[3],[7,4],[2,4,6]]            `shouldBe`  14
  it "e3" $
    maximaSuma [[3],[7,4],[2,4,6],[8,5,9,3]]  `shouldBe`  23

spec :: Spec
spec = do
  describe "def. 1" $ specG maximaSuma1
  describe "def. 2" $ specG maximaSuma2
  describe "def. 3" $ specG maximaSuma3
  describe "def. 4" $ specG maximaSuma4
  describe "def. 5" $ specG maximaSuma5
  describe "def. 6" $ specG maximaSuma6
  describe "def. 7" $ specG maximaSuma7
  describe "def. 8" $ specG maximaSuma8

-- La verificación es
--    λ> verifica
--    Finished in 0.0053 seconds
--    24 examples, 0 failures
