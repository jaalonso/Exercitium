-- El_tipo_de_los_numeros_naturales.hs
-- El tipo de los números naturales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El tipo de los números raturales se puede definir por
--    data Nat = Cero | Suc Nat
--      deriving (Show, Eq)
-- de forma que (Suc (Suc (Suc Cero))) representa el número 3.
--
-- Definir las siguientes funciones
--    nat2int :: Nat -> Int
--    int2nat :: Int -> Nat
--    suma    :: Nat -> Nat -> Nat
-- tales que
-- + (nat2int n) es el número entero correspondiente al número natural
--   n. Por ejemplo,
--      nat2int (Suc (Suc (Suc Cero)))  ==  3
-- + (int2nat n) es el número natural correspondiente al número entero n. Por ejemplo,
--      int2nat 3  ==  Suc (Suc (Suc Cero))
-- + (suma m n) es la suma de los número naturales m y n. Por ejemplo,
--      λ> suma (Suc (Suc Cero)) (Suc Cero)
--      Suc (Suc (Suc Cero))
--      λ> nat2int (suma (Suc (Suc Cero)) (Suc Cero))
--      3
--      λ> nat2int (suma (int2nat 2) (int2nat 1))
--      3
-- ---------------------------------------------------------------------

module El_tipo_de_los_numeros_naturales where

data Nat = Cero | Suc Nat
  deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Cero    = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Cero
int2nat n = Suc (int2nat (n-1))

suma :: Nat -> Nat -> Nat
suma Cero    n = n
suma (Suc m) n = Suc (suma m n)
