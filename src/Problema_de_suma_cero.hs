-- Problema_de_suma_cero.hs
-- Problema de suma cero.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-Septiembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema de suma cero consiste en, dado el conjunto de números
-- enteros, encontrar sus subconjuntos no vacío cuyos elementos sumen
-- cero.
--
-- Usando el [procedimiento de búsqueda en profundidad](https://bit.ly/3NPI4qV),
-- definir la función
--    suma0 :: [Int] -> [[Int]]
-- tal que (suma0 ns) es la lista de las soluciones del problema de suma
-- cero para ns. Por ejemplo,
--    λ> suma0 [-7,-3,-2,5,8]
--    [[-3,-2,5]]
--    λ> suma0 [-7,-3,-2,5,8,-1]
--    [[-7,-3,-2,-1,5,8],[-7,-1,8],[-3,-2,5]]
--    λ> suma0 [-7,-3,1,5,8]
--    []
-- ---------------------------------------------------------------------

module Problema_de_suma_cero where

import BusquedaEnProfundidad (buscaProfundidad)
import Data.List (delete, nub, sort)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- Los estados son ternas formadas por los números seleccionados, su
-- suma y los restantes números.
type Estado = ([Int], Int, [Int])

inicial :: [Int] -> Estado
inicial ns = ([],0,ns)

esFinal :: Estado -> Bool
esFinal (xs,s,_) =
  not (null xs) && s == 0

sucesores :: Estado -> [Estado]
sucesores (xs,s,ns) =
  [(n:xs, n+s, delete n ns) | n <- ns]

soluciones :: [Int] -> [Estado]
soluciones ns =
  buscaProfundidad sucesores esFinal (inicial ns)

suma0 :: [Int] -> [[Int]]
suma0 ns =
  nub [sort xs | (xs,_,_) <- soluciones ns]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    suma0 [-7,-3,-2,5,8] `shouldBe`
    [[-3,-2,5]]
  it "e2" $
    suma0 [-7,-3,-2,5,8,-1] `shouldBe`
    [[-7,-3,-2,-1,5,8],[-7,-1,8],[-3,-2,5]]
  it "e3" $
    suma0 [-7,-3,1,5,8] `shouldBe`
    []

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.0098 seconds
--    3 examples, 0 failures
