-- Mayor_capicua_producto_de_dos_numeros_de_n_cifras.hs
-- Mayor capicúa producto de dos números de n cifras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 03-Febrero-2015 (actualizado 25-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un capicúa es un número que es igual leído de izquierda a derecha que
-- de derecha a izquierda.
--
-- Definir la función
--    mayorCapicuaP :: Integer -> Integer
-- tal que (mayorCapicuaP n) es el mayor capicúa que es el producto de
-- dos números de n cifras. Por ejemplo,
--    mayorCapicuaP 2  ==  9009
--    mayorCapicuaP 3  ==  906609
--    mayorCapicuaP 4  ==  99000099
--    mayorCapicuaP 5  ==  9966006699
--    mayorCapicuaP 6  ==  999000000999
--    mayorCapicuaP 7  ==  99956644665999
-- ---------------------------------------------------------------------

module Mayor_capicua_producto_de_dos_numeros_de_n_cifras where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

mayorCapicuaP1 :: Integer -> Integer
mayorCapicuaP1 n = maximum [x*y | x <- [a,a-1..b],
                                  y <- [a,a-1..b],
                                  esCapicua (x*y)]
  where a = 10^n-1
        b = 10^(n-1)

-- (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 353  ==  True
--    esCapicua 357  ==  False
esCapicua :: Integer -> Bool
esCapicua n = xs == reverse xs
  where xs = show n

-- 2ª solución
-- ===========

mayorCapicuaP2 :: Integer -> Integer
mayorCapicuaP2 n = maximum [x*y | (x,y) <- pares a b,
                                  esCapicua (x*y)]
  where a = 10^n-1
        b = 10^(n-1)

-- (pares a b) es la lista de los pares de números entre a y b de forma
-- que su suma es decreciente. Por ejemplo,
--    pares 9 7  ==  [(9,9),(8,9),(8,8),(7,9),(7,8),(7,7)]
pares :: Integer -> Integer -> [(Integer,Integer)]
pares a b = [(x,z-x) | z <- [a1,a1-1..b1],
                        x <- [a,a-1..b],
                        x <= z-x, z-x <= a]
  where a1 = 2*a
        b1 = 2*b

-- 3ª solución
-- ===========

mayorCapicuaP3 :: Integer -> Integer
mayorCapicuaP3 n = head (capicuasP n)

-- (capicuasP n) es la lista de las capicúas de 2*n cifras que
-- pueden escribirse como productos de dos números de n cifras. Por
-- ejemplo, Por ejemplo,
--    ghci> capicuasP 2
--    [9009,8448,8118,8008,7227,7007,6776,6336,6006,5775,5445,5335,
--     5225,5115,5005,4884,4774,4664,4554,4224,4004,3773,3663,3003,
--     2992,2772,2552,2442,2332,2112,2002,1881,1771,1551,1221,1001]
capicuasP :: Integer -> [Integer]
capicuasP n = [x | x <- capicuas n,
                        not (null (productosDosNumerosCifras n x))]

-- (capicuas n) es la lista de las capicúas de 2*n cifras de mayor a
-- menor. Por ejemplo,
--    capicuas 1           ==  [99,88,77,66,55,44,33,22,11]
--    take 7 (capicuas 2)  ==  [9999,9889,9779,9669,9559,9449,9339]
capicuas :: Integer -> [Integer]
capicuas n = [capicua x | x <- numerosCifras n]

-- (numerosCifras n) es la lista de los números de n cifras de mayor a
-- menor. Por ejemplo,
--    numerosCifras 1           ==  [9,8,7,6,5,4,3,2,1]
--    take 7 (numerosCifras 2)  ==  [99,98,97,96,95,94,93]
--    take 7 (numerosCifras 3)  ==  [999,998,997,996,995,994,993]
numerosCifras :: Integer -> [Integer]
numerosCifras n = [a,a-1..b]
  where a = 10^n-1
        b = 10^(n-1)

-- (capicua n) es la capicúa formada añadiendo el inverso de n a
--  continuación de n. Por ejemplo,
--    capicua 93  ==  9339
capicua :: Integer -> Integer
capicua n = read (xs ++ reverse xs)
  where xs = show n

-- (productosDosNumerosCifras n x) es la lista de los números y de n
-- cifras tales que existe un z de n cifras y x es el producto de y por
-- z. Por ejemplo,
--    productosDosNumerosCifras 2 9009  ==  [99,91]
productosDosNumerosCifras :: Integer -> Integer -> [Integer]
productosDosNumerosCifras n x = [y | y <- numeros,
                                     mod x y == 0,
                                     div x y `elem` numeros]
  where numeros = numerosCifras n

-- 4ª solución
-- ===========

mayorCapicuaP4 :: Integer -> Integer
mayorCapicuaP4 n = buscar maxN 0
  where
    minN = 10^(n-1)
    maxN = 10^n - 1
    buscar x actualMax
      | x < minN = actualMax
      | x * x <= actualMax = actualMax
      | otherwise = buscar (x - 1) (buscarY x x actualMax)
    buscarY x y m
      | y < minN || x * y <= m = m
      | esCapicua (x * y) = x * y
      | otherwise = buscarY x (y - 1) m


-- 5ª solución
-- ===========

mayorCapicuaP5 :: Integer -> Integer
mayorCapicuaP5 n = head [x | x <- capicuas n, esFactorizable x n]

-- (esFactorizable x n) se verifica si x se puede escribir como producto
-- de dos números de n dígitos. Por ejemplo,
--    esFactorizable 1219 2  ==  True
--    esFactorizable 1217 2  ==  False
esFactorizable :: Integer -> Integer -> Bool
esFactorizable x n = aux m x
  where b = 10^n-1
        m = floor (sqrt (fromIntegral x))
        aux i z | i > b          = False
                | z `mod` i == 0 = z `div` i < b
                | otherwise      = aux (i+1) z


-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Integer -> Integer) -> Spec
specG mayorCapicuaP = do
  it "e1" $
    mayorCapicuaP 2  `shouldBe`  9009

spec :: Spec
spec = do
  describe "def. 1" $ specG mayorCapicuaP1
  describe "def. 2" $ specG mayorCapicuaP2
  describe "def. 3" $ specG mayorCapicuaP3
  describe "def. 4" $ specG mayorCapicuaP4
  describe "def. 5" $ specG mayorCapicuaP5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mayorCapicuaP1 3
--    906609
--    (0.58 secs, 555,907,856 bytes)
--    λ> mayorCapicuaP2 3
--    906609
--    (1.02 secs, 719,446,128 bytes)
--    λ> mayorCapicuaP3 3
--    906609
--    (0.07 secs, 18,734,280 bytes)
--    λ> mayorCapicuaP4 3
--    906609
--    (0.04 secs, 6,654,976 bytes)
--    λ> mayorCapicuaP5 3
--    906609
--    (0.02 secs, 1,947,392 bytes)
--
--    λ> mayorCapicuaP3 4
--    99000099
--    (0.37 secs, 186,791,768 bytes)
--    λ> mayorCapicuaP4 4
--    99000099
--    (0.01 secs, 3,414,584 bytes)
--    λ> mayorCapicuaP5 4
--    99000099
--    (0.03 secs, 2,165,400 bytes)
--
--    λ> mayorCapicuaP4 5
--    9966006699
--    (1.35 secs, 1,220,213,152 bytes)
--    λ> mayorCapicuaP5 5
--    9966006699
--    (0.07 secs, 12,344,088 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en el [problema 2](http://bit.ly/1zcKPGz) del proyecto Euler.
