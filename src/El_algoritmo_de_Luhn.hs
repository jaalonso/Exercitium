-- El_algoritmo_de_Luhn.hs
-- El algoritmo de Luhn.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-noviembre-2022
-- ---------------------------------------------------------------------

module El_algoritmo_de_Luhn where

-- ---------------------------------------------------------------------
-- El objetivo de este ejercicio es estudiar un algoritmo para validar
-- algunos identificadores numéricos como los números de algunas tarjetas
-- de crédito; por ejemplo, las de tipo Visa o Master Card.
--
-- El algoritmo que vamos a estudiar es el [algoritmo de
-- Luhn](https://bit.ly/3DX1llv) consistente en aplicar los siguientes
-- pasos a los dígitos del número de la tarjeta.
--    1. Se invierten los dígitos del número; por ejemplo, [9,4,5,5] se
--       transforma en [5,5,4,9].
--    2. Se duplican los dígitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los dígitos de cada número; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el último dígito de la suma es 0, el número es válido; y no
--       lo es, en caso contrario.
--
-- A los números válidos, se les llama números de Luhn.
--
-- Definir las siguientes funciones:
--    digitosInv    :: Integer -> [Integer]
--    doblePosImpar :: [Integer] -> [Integer]
--    sumaDigitos   :: [Integer] -> Integer
--    ultimoDigito  :: Integer -> Integer
--    luhn          :: Integer -> Bool
-- tales que
-- + (digitosInv n) es la lista de los dígitos del número n. en orden
--   inverso. Por ejemplo,
--      digitosInv 320274  ==  [4,7,2,0,2,3]
-- + (doblePosImpar ns) es la lista obtenida doblando los elementos en
--   las posiciones impares (empezando a contar en cero y dejando igual
--   a los que están en posiciones pares. Por ejemplo,
--      doblePosImpar [4,9,5,5]    ==  [4,18,5,10]
--      doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- + (sumaDigitos ns) es la suma de los dígitos de ns. Por ejemplo,
--      sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                              = 19
-- + (ultimoDigito n) es el último dígito de n. Por ejemplo,
--      ultimoDigito 123 == 3
--      ultimoDigito   0 == 0
-- + (luhn n) se verifica si n es un número de Luhn. Por ejemplo,
--      luhn 5594589764218858  ==  True
--      luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------

-- Definición de digitosInv
-- ========================

digitosInv :: Integer -> [Integer]
digitosInv n = [read [x] | x <- reverse (show n)]

-- Nota: En el ejercicio "Dígitos de un número" https://bit.ly/3Tkhc2T
-- se presentan otras definiciones.

-- Definiciones de doblePosImpar
-- =============================

-- 1ª definición
doblePosImpar :: [Integer] -> [Integer]
doblePosImpar []       = []
doblePosImpar [x]      = [x]
doblePosImpar (x:y:zs) = x : 2*y : doblePosImpar zs

-- 2ª definición
doblePosImpar2 :: [Integer] -> [Integer]
doblePosImpar2 (x:y:zs) = x : 2*y : doblePosImpar2 zs
doblePosImpar2 xs       = xs

-- 3ª definición
doblePosImpar3 :: [Integer] -> [Integer]
doblePosImpar3 xs = [f n x | (n,x) <- zip [0..] xs]
  where f n x | odd n     = 2*x
              | otherwise = x

-- Definiciones de sumaDigitos
-- ===========================

sumaDigitos :: [Integer] -> Integer
sumaDigitos ns = sum [sum (digitosInv n) | n <- ns]

-- Nota: En el ejercicio "Suma de los dígitos de un número"
-- https://bit.ly/3U4u7WR se presentan otras definiciones.

-- Definición de ultimoDigito
-- ==========================

ultimoDigito :: Integer -> Integer
ultimoDigito n = n `rem` 10

-- Definiciones de luhn
-- ====================

-- 1ª definición
luhn1 :: Integer -> Bool
luhn1 n =
  ultimoDigito (sumaDigitos (doblePosImpar (digitosInv n))) == 0

-- 2ª definición
luhn2 :: Integer -> Bool
luhn2 =
  (==0) . ultimoDigito . sumaDigitos . doblePosImpar . digitosInv
