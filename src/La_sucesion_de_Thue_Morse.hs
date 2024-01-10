-- La_sucesion_de_Thue_Morse.hs
-- La sucesión de Thue-Morse.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-enero-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La serie de Thue-Morse comienza con el término [0] y sus siguientes
-- términos se construyen añadiéndole al anterior su complementario. Los
-- primeros términos de la serie son
--    [0]
--    [0,1]
--    [0,1,1,0]
--    [0,1,1,0,1,0,0,1]
--    [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0]
-- De esta forma se va formando una sucesión
--    0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,...
-- que se conoce como la [sucesión de Thue-Morse](https://bit.ly/3PE9LRJ).
--
-- Definir la sucesión
--    sucThueMorse :: [Int]
-- cuyos elementos son los de la sucesión de Thue-Morse. Por ejemplo,
--    λ> take 30 sucThueMorse
--    [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0]
--    λ> map (sucThueMorse4 !!) [1234567..1234596]
--    [1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0]
--    λ> map (sucThueMorse4 !!) [4000000..4000030]
--    [1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1]
--
-- Comprobar con QuickCheck que si s(n) representa el término n-ésimo de
-- la sucesión de Thue-Morse, entonces
--    s(2n)   = s(n)
--    s(2n+1) = 1 - s(n)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module La_sucesion_de_Thue_Morse where

import Test.QuickCheck
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª solución
-- ===========

sucThueMorse1 :: [Int]
sucThueMorse1 = map termSucThueMorse1 [0..]

-- (termSucThueMorse1 n) es el n-ésimo término de la sucesión de
-- Thue-Morse. Por ejemplo,
--    termSucThueMorse1 0  ==  0
--    termSucThueMorse1 1  ==  1
--    termSucThueMorse1 2  ==  1
--    termSucThueMorse1 3  ==  0
--    termSucThueMorse1 4  ==  1
termSucThueMorse1 :: Int -> Int
termSucThueMorse1 0 = 0
termSucThueMorse1 n =
  (serieThueMorse !! k) !! n
  where k = 1 + floor (logBase 2 (fromIntegral n))

-- serieThueMorse es la lista cuyos elementos son los términos de la
-- serie de Thue-Morse. Por ejemplo,
--    λ> take 4 serieThueMorse3
--    [[0],[0,1],[0,1,1,0],[0,1,1,0,1,0,0,1]]
serieThueMorse :: [[Int]]
serieThueMorse = iterate paso [0]
  where paso xs = xs ++ map (1-) xs

-- 2ª solución
-- ===========

sucThueMorse2 :: [Int]
sucThueMorse2 =
  0 : intercala (map (1-) sucThueMorse2) (tail sucThueMorse2)

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- las listas infinitas xs e ys. Por ejemplo,
--    take 10 (intercala [1,5..] [2,4..])  ==  [1,2,5,4,9,6,13,8,17,10]
intercala :: [a] -> [a] -> [a]
intercala (x:xs) ys = x : intercala ys xs

-- 3ª solución
-- ===========

sucThueMorse3 :: [Int]
sucThueMorse3 = 0 : 1 : aux (tail sucThueMorse3)
  where aux (x : xs) = x : (1 - x) : aux xs

-- 4ª solución
-- ===========

sucThueMorse4 :: [Int]
sucThueMorse4 = 0 : aux [1]
  where aux xs = xs ++ aux (xs ++ map (1-) xs)

-- Comprobación de la propiedad
-- ============================

-- La propiedad es
prop_termSucThueMorse :: NonNegative Int -> Bool
prop_termSucThueMorse (NonNegative n) =
  sucThueMorse1 !! (2*n)   == sn &&
  sucThueMorse1 !! (2*n+1) == 1 - sn
  where sn = sucThueMorse1 !! n

-- La comprobación es
--    λ> quickCheck prop_termSucThueMorse
--    +++ OK, passed 100 tests.

-- 5ª solución
-- ===========

sucThueMorse5 :: [Int]
sucThueMorse5 = map termSucThueMorse5 [0..]

-- (termSucThueMorse5 n) es el n-ésimo término de la sucesión de
-- Thue-Morse. Por ejemplo,
--    termSucThueMorse5 0  ==  0
--    termSucThueMorse5 1  ==  1
--    termSucThueMorse5 2  ==  1
--    termSucThueMorse5 3  ==  0
--    termSucThueMorse5 4  ==  1
termSucThueMorse5 :: Int -> Int
termSucThueMorse5 0 = 0
termSucThueMorse5 n
  | even n    = termSucThueMorse5 (n `div` 2)
  | otherwise = 1 - termSucThueMorse5 (n `div` 2)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Int] -> Spec
specG sucThueMorse = do
  it "e1" $
    take 30 sucThueMorse `shouldBe`
    [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,0,1,1,0,1,0]

spec :: Spec
spec = do
  describe "def. 1" $ specG sucThueMorse1
  describe "def. 2" $ specG sucThueMorse2
  describe "def. 3" $ specG sucThueMorse3
  describe "def. 4" $ specG sucThueMorse4
  describe "def. 5" $ specG sucThueMorse5

-- La verificación es
--    λ> verifica
--
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_sucThueMorse :: NonNegative Int -> Bool
prop_sucThueMorse (NonNegative n) =
  all (== sucThueMorse1 !! n)
      [sucThueMorse2 !! n,
       sucThueMorse3 !! n,
       sucThueMorse4 !! n,
       sucThueMorse5 !! n]

-- La comprobación es
--    λ> quickCheck prop_sucThueMorse
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> sucThueMorse1 !! (10^7)
--    0
--    (3.28 secs, 3,420,080,168 bytes)
--    λ> sucThueMorse2 !! (10^7)
--    0
--    (3.01 secs, 1,720,549,640 bytes)
--    λ> sucThueMorse3 !! (10^7)
--    0
--    (1.80 secs, 1,360,550,040 bytes)
--    λ> sucThueMorse4 !! (10^7)
--    0
--    (0.88 secs, 1,254,772,768 bytes)
--    λ> sucThueMorse5 !! (10^7)
--    0
--    (0.62 secs, 1,600,557,072 bytes)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + N.J.A. Sloane "Sucesión A010060" en OEIS http://oeis.org/A010060
-- + Programming Praxis "Thue-Morse sequence" http://bit.ly/1n2PdFk
-- + Wikipedia "Thue–Morse sequence" http://bit.ly/1KvZONW
