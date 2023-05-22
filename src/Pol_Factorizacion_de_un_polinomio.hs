-- Pol_Factorizacion_de_un_polinomio.hs
-- TAD de los polinomios: Factorización de un polinomio.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 22-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de los polinomios](https://bit.ly/3KwqXYu),
-- definir la función
--    factorizacion :: Polinomio Int -> [Polinomio Int]
-- tal que (factorizacion p) es la lista de la descomposición del
-- polinomio p en factores obtenida mediante el regla de Ruffini. Por
-- ejemplo,
--    λ> ejPol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
--    λ> ejPol1
--    x^5 + 5*x^2 + 4*x
--    λ> factorizacion ejPol1
--    [1*x,1*x + 1,x^3 + -1*x^2 + 1*x + 4]
--    λ> ejPol2 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))
--    λ> ejPol2
--    x^3 + 2*x^2 + -1*x + -2
--    λ> factorizacion ejPol2
--    [1*x + -1,1*x + 1,1*x + 2,1]
-- ---------------------------------------------------------------------

module Pol_Factorizacion_de_un_polinomio where

import TAD.Polinomio (Polinomio, consPol, polCero, esPolCero)
import Pol_Termino_independiente_de_un_polinomio (terminoIndep)
import Pol_Raices_enteras_de_un_polinomio (divisores)
import Pol_Regla_de_Ruffini (cocienteRuffini)
import Pol_Reconocimiento_de_raices_por_la_regla_de_Ruffini (esRaizRuffini)
import Pol_Transformaciones_polinomios_densas (densaApolinomio)
import Test.Hspec (Spec, hspec, it, shouldBe)

factorizacion :: Polinomio Int -> [Polinomio Int]
factorizacion p
  | esPolCero p = [p]
  | otherwise   = aux (0 : divisores (terminoIndep p))
  where
    aux [] = [p]
    aux (r:rs)
        | esRaizRuffini r p =
            densaApolinomio [1,-r] : factorizacion (cocienteRuffini r p)
        | otherwise = aux rs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    map show (factorizacion ejPol1)
      `shouldBe` ["1*x","1*x + 1","x^3 + -1*x^2 + 1*x + 4"]
  it "e2" $
    map show (factorizacion ejPol2)
      `shouldBe` ["1*x + -1","1*x + 1","1*x + 2","1"]
  where
    ejPol1 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
    ejPol2 = consPol 3 1 (consPol 2 2 (consPol 1 (-1) (consPol 0 (-2) polCero)))

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0015 seconds
--    2 examples, 0 failures
