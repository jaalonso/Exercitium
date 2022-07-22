-- La_sucesion_de_Ducci.hs
-- La sucesión de Ducci.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, ??-julio-2022
-- ---------------------------------------------------------------------

module La_sucesion_de_Ducci where

import Test.QuickCheck
import Data.List (tails)

--    [3, 10, 2, 15]
--    [7, 8, 13, 12]
--    [1, 5, 1, 5]
--    [4, 4, 4, 4]
--    [0, 0, 0, 0]

siguienteDucci :: [Integer] -> [Integer]
siguienteDucci xs =
  [abs (x - y) | (x,y) <- zip xs (tail xs)] ++ [abs (last xs - head xs)]


ducci :: [Integer] -> [[Integer]]
ducci = iterate siguienteDucci

ducciHastaCero :: [Integer] -> [[Integer]]
ducciHastaCero xs =
  takeWhile (/= cero) (ducci xs)
  where n = length xs
        cero = replicate n 0

-- ---------------------------------------------------------------------
-- § Teorema de Ducci                                                 --
-- ---------------------------------------------------------------------

newtype ListaDucci = LD [Integer]
  deriving Show

--    λ> generate (listaDucciArbitraria 2)
--    LD [24,-2,14,26]
listaDucciArbitraria :: Int -> Gen ListaDucci
listaDucciArbitraria n = do
  xs <- vector (2^n)
  return (LD xs)

instance Arbitrary ListaDucci where
  arbitrary = sized listaDucciArbitraria


prop_teorema_Ducci :: ListaDucci -> Bool
prop_teorema_Ducci (LD xs) =
  replicate n 0 `elem` ducci xs
  where n = length xs

-- λ> quickCheckWith (stdArgs {maxSize=10}) prop_teorema_Ducci
-- +++ OK, passed 100 tests.

tribonacci :: Int -> Integer
tribonacci 0 = 0
tribonacci 1 = 0
tribonacci 2 = 1
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

--    λ> take 15 tribonaccis
--    [0,0,1,1,2,4,7,13,24,44,81,149,274,504,927]
tribonaccis :: [Integer]
tribonaccis = map tribonacci [0..]

--    λ> take 15 tribonaccis2
--    [0,0,1,1,2,4,7,13,24,44,81,149,274,504,927]
tribonaccis2 :: [Integer]
tribonaccis2 =
  0 : 0 : 1 : zipWith3 suma3 tribonaccis (tail tribonaccis) (drop 2 tribonaccis)
  where suma3 x y z = x + y + z

longitudDucciTribonacci :: [Int]
longitudDucciTribonacci =
  [length (ducciHastaCero xs) |
   xs <- take 4 (tails tribonaccis2)]
  
-- Referencias
-- ===========

-- + [Ducci sequences](https://bit.ly/3P3Y7zK) por John D. Cook.
-- + [Ducci sequence](https://bit.ly/3PpTakG) en Wikipedia.
-- + [El teorema de Ducci](https://bit.ly/3aBrzOz) por Marta Macho
--   Stadler. 
