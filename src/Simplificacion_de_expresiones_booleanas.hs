-- Simplificacion_de_expresiones_booleanas.hs
-- Simplificación de expresiones booleanas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-Enero-2015 (actualizado 23-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El siguiente tipo de dato algebraico representa las fórmulas
-- proposicionales construidas con una variable (X), las constantes
-- verdadera (V) y falsa (F), la negación (Neg) y la disyunción (Dis):
--    data Form = X
--              | V
--              | F
--              | Neg Form
--              | Dis Form Form
--      deriving (Eq, Ord, Show)
-- Por ejemplo, la fórmula (¬X v V) se representa por (Dis (Neg X) V).
--
-- Definir las funciones
--    valor      :: Form -> Bool -> Bool
--    simplifica :: Form -> Form
-- tales que (valor p i) es el valor de la fórmula p cuando se le asigna
-- a X el valor i. Por ejemplo,
--    valor (Neg X) True           ==  False
--    valor (Neg F) True           ==  True
--    valor (Dis X (Neg X)) True   ==  True
--    valor (Dis X (Neg X)) False  ==  True
-- y (simplifica p) es una expresión obtenida aplicándole a p las siguientes
-- reglas de simplificación:
--    Neg V       = F
--    Neg F       = V
--    Neg (Neg q) = q
--    Dis V q     = V
--    Dis F q     = q
--    Dis q V     = V
--    Dis q F     = q
--    Dis q q     = q
-- Por ejemplo,
--    simplifica (Dis X (Neg (Neg X)))                      ==  X
--    simplifica (Neg (Dis (Neg (Neg X)) F))                ==  Neg X
--    simplifica (Dis (Neg F) F)                            ==  V
--    simplifica (Dis (Neg V) (Neg (Dis (Neg X) F)))        ==  X
--    simplifica (Dis (Neg V) (Neg (Dis (Neg (Neg X)) F)))  ==  Neg X
--
-- Comprobar con QuickCheck que para cualquier fórmula p y cualquier
-- booleano i se verifica que (valor (simplifica p) i) es igual a
-- (valor p i).
-- ---------------------------------------------------------------------

module Simplificacion_de_expresiones_booleanas where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

data Form = X
          | V
          | F
          | Neg Form
          | Dis Form Form
  deriving (Eq, Ord, Show)

-- 1ª solución
-- ===========

valor :: Form -> Bool -> Bool
valor X i         = i
valor V _         = True
valor F _         = False
valor (Neg p) i   = not (valor p i)
valor (Dis p q) i = valor p i || valor q i

simplifica1 :: Form -> Form
simplifica1 X = X
simplifica1 V = V
simplifica1 F = F
simplifica1 (Neg p) = simplificaNeg (simplifica1 p)
simplifica1 (Dis p q) = simplificaDis (simplifica1 p) (simplifica1 q)

simplificaNeg :: Form -> Form
simplificaNeg V       = F
simplificaNeg F       = V
simplificaNeg (Neg p) = p
simplificaNeg p       = Neg p

simplificaDis :: Form -> Form -> Form
simplificaDis V _ = V
simplificaDis F q = q
simplificaDis _ V = V
simplificaDis q F = q
simplificaDis p q | p == q    = p
                  | otherwise = Dis p q

-- 2ª solución
-- ===========

simplifica2 :: Form -> Form
simplifica2 X = X
simplifica2 V = V
simplifica2 F = F
simplifica2 (Neg p)   = regla (Neg (simplifica2 p))
simplifica2 (Dis p q) = regla (Dis (simplifica2 p) (simplifica2 q))

regla :: Form -> Form
regla (Neg V)              = F
regla (Neg F)              = V
regla (Neg (Neg q))        = q
regla (Dis V _)            = V
regla (Dis _ V)            = V
regla (Dis F q)            = q
regla (Dis q F)            = q
regla (Dis q r) | q == r   = q
regla f                    = f

-- 3ª solución
-- ===========

simplifica3 :: Form -> Form
simplifica3 X = X
simplifica3 V = V
simplifica3 F = F
simplifica3 (Neg p) =
  case simplifica3 p of
    V     -> F
    F     -> V
    Neg q -> q
    p'    -> Neg p'
simplifica3 (Dis p q) =
  case (simplifica3 p, simplifica3 q) of
    (V, _)               -> V
    (_, V)               -> V
    (F, q')              -> q'
    (p', F)              -> p'
    (p', q') | p'==q'    -> p'
             | otherwise -> Dis p' q'

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG1 :: (Form -> Bool -> Bool) -> Spec
specG1 valor' = do
  it "e1" $
    valor' (Neg X) True           `shouldBe`  False
  it "e2" $
    valor' (Neg F) True           `shouldBe`  True
  it "e3" $
    valor' (Dis X (Neg X)) True   `shouldBe`  True
  it "e4" $
    valor' (Dis X (Neg X)) False  `shouldBe`  True

specG2 :: (Form -> Form) -> Spec
specG2 simplifica = do
  it "e5" $
    simplifica (Dis X (Neg (Neg X)))                      `shouldBe`  X
  it "e6" $
    simplifica (Neg (Dis (Neg (Neg X)) F))                `shouldBe`  Neg X
  it "e7" $
    simplifica (Dis (Neg F) F)                            `shouldBe`  V
  it "e8" $
    simplifica (Dis (Neg V) (Neg (Dis (Neg X) F)))        `shouldBe`  X
  it "e9" $
    simplifica (Dis (Neg V) (Neg (Dis (Neg (Neg X)) F)))  `shouldBe`  Neg X

spec :: Spec
spec = do
  describe "def. 1" $ specG1 valor
  describe "def. 2" $ specG2 simplifica1
  describe "def. 3" $ specG2 simplifica2
  describe "def. 4" $ specG2 simplifica3

-- La verificación es
--    λ> verifica
--    24 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- (genForm n) es un generador de fórmulas de complejidad menor o igual
-- que n. Por ejemplo,
--    λ> generate (genForm 5)
--    Dis (Dis X F) (Dis (Dis F F) (Dis V X))
--    λ> generate (genForm 5)
--    Dis (Neg V) (Neg (Dis F X))
genForm :: Int -> Gen Form
genForm 0 = elements [X, V, F]
genForm n = oneof [elements [X, V, F],
                   Neg <$> genForm (n-1),
                   Dis <$> genForm (n `div` 2) <*> genForm (n `div` 2)]

instance Arbitrary Form where
  arbitrary = sized genForm

-- La propiedad es
prop_equivalencia :: Form -> Bool
prop_equivalencia p =
    all (== simplifica1 p)
        [simplifica2 p,
         simplifica3 p]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

ejemplo :: Int -> Form
ejemplo 0 = X
ejemplo n
  | even n    = Neg (ejemplo (n-1))
  | otherwise = Dis (ejemplo (n-1)) F

-- La comparación es
--    λ> :set +s
--    λ> simplifica1 (ejemplo (10^6))
--    X
--    (1.12 secs, 642,260,816 bytes)
--    λ> simplifica2 (ejemplo (10^6))
--    X
--    (1.32 secs, 724,409,416 bytes)
--    λ> simplifica3 (ejemplo (10^6))
--    X
--    (1.07 secs, 642,422,136 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_simplifica :: Form -> Bool -> Bool
prop_simplifica p i =
  valor (simplifica1 p) i == valor p i

-- La comprobación es
--    λ> quickCheck prop_simplifica
--    +++ OK, passed 100 tests.
