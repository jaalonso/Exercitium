-- Mayor_numero_con_dos_digitos_dados.hs
-- Mayor número con dos dígitos dados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-septiembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    numeroMayor :: Int -> Int -> Int
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

module Mayor_numero_con_dos_digitos_dados where

-- 1ª definición:
numeroMayor1 :: Int -> Int -> Int
numeroMayor1 x y = 10 * max x y + min x y

-- 2ª definición:
numeroMayor2 :: Int -> Int -> Int
numeroMayor2 x y | x > y     = 10*x+y
                 | otherwise = 10*y+x

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_numeroMayor :: Bool
prop_numeroMayor =
  and [numeroMayor1 x y == numeroMayor2 x y | x <- [0..9], y <- [0..9]]

-- La comprobación es
--    λ> prop_numeroMayor
--    True
