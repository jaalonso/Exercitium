-- Producto_de_los_elementos_de_la_diagonal_principal.hs
-- Producto de los elementos de la diagonal principal.
-- José A. Alonso Jiménez
-- Sevilla, 3-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices se pueden representar como lista de listas de la misma
-- longitud, donde cada uno de sus elementos representa una fila de la
-- matriz.
--
-- Definir la función
--    productoDiagonalPrincipal :: Num a => [[a]] -> a
-- tal que (productoDiagonalPrincipal xss) es el producto de los
-- elementos de la diagonal principal de la matriz cuadrada xss. Por
-- ejemplo,
--    productoDiagonal1 [[3,5,2],[4,7,1],[6,9,8]]  ==  168
--    productoDiagonal1 (replicate 5 [1..5])       ==  120
--    length (show (productoDiagonal3 (replicate 30000 [1..30000])))  ==  121288
-- ---------------------------------------------------------------------

module Producto_de_los_elementos_de_la_diagonal_principal where

import Data.List (genericReplicate)
import Test.Hspec

-- 1ª solución
-- ===========

productoDiagonal1 :: Num a => [[a]] -> a
productoDiagonal1 xss = product (diagonal1 xss)

-- (diagonal1 xss) es la diagonal de la matriz xss. Por ejemplo,
--    diagonal1 [[3,5,2],[4,7,1],[6,9,0]]  ==  [3,7,0]
--    diagonal1 [[3,5],[4,7],[6,9]]        ==  [3,7]
--    diagonal1 [[3,5,2],[4,7,1]]          ==  [3,7]
diagonal1 :: [[a]] -> [a]
diagonal1 ((x:_):xss) = x : diagonal1 (map tail xss)
diagonal1 _           = []

-- 2ª solución
-- ===========

productoDiagonal2 :: Num a => [[a]] -> a
productoDiagonal2 = product . diagonal1

-- 3ª solución
-- ===========

productoDiagonal3 :: Num a => [[a]] -> a
productoDiagonal3 = product . diagonal3

diagonal3 :: [[a]] -> [a]
diagonal3 xss = [xs!!k | (xs,k) <- zip xss [0..n]]
  where n = length (head xss) - 1

-- 4ª solución
-- ===========

productoDiagonal4 :: Num a => [[a]] -> a
productoDiagonal4 []          = 1
productoDiagonal4 ((x:_):xss) = x * productoDiagonal4 (map tail xss)
productoDiagonal4 _ = error "Imposible"

-- 5ª solución
-- ===========

productoDiagonal5 :: Num a => [[a]] -> a
productoDiagonal5 xss = product (zipWith (!!) xss [0..])

-- Comparación de eficiencia
-- =========================

ejemplo :: Integer -> [[Integer]]
ejemplo n = genericReplicate n [1..n]

-- La comparación es
--    λ> length (show (productoDiagonal1 (ejemplo 7000)))
--    23878
--    (1.23 secs, 3,396,129,424 bytes)
--    λ> length (show (productoDiagonal2 (ejemplo 7000)))
--    23878
--    (0.94 secs, 3,396,127,680 bytes)
--    λ> length (show (productoDiagonal3 (ejemplo 7000)))
--    23878
--    (0.09 secs, 44,841,864 bytes)
--    λ> length (show (productoDiagonal4 (ejemplo 7000)))
--    23878
--    (0.96 secs, 3,614,137,840 bytes)
--    λ> length (show (productoDiagonal5 (ejemplo 7000)))
--    23878
--    (0.07 secs, 44,168,984 bytes)
--
--    λ> length (show (productoDiagonal3 (ejemplo 70000)))
--    308760
--    (8.26 secs, 5,359,752,408 bytes)
--    λ> length (show (productoDiagonal5 (ejemplo 70000)))
--    308760
--    (9.34 secs, 5,353,035,656 bytes)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica :: ([[Integer]] -> Integer) -> IO ()
verifica productoDiagonal = hspec $ do
  it "e1" $
    productoDiagonal [[1]]                     `shouldBe` 1
  it "e2" $
    productoDiagonal [[1,2,3],[4,5,6],[7,8,9]]  `shouldBe` 1 * 5 * 9
  it "e3" $
    productoDiagonal [[3,5,2],[4,7,1],[6,9,0]]  `shouldBe` 3 * 7 * 0
  it "e4" $
    productoDiagonal [[3,5],[4,7],[6,9]]        `shouldBe` 3 * 7
  it "e5" $
    productoDiagonal [[3,5,2],[4,7,1]]          `shouldBe` 3 * 7
