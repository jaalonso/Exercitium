-- Algoritmo_de_bajada.hs
-- Algoritmo de bajada para resolver un sistema triangular inferior.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-diciembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un sistema de ecuaciones lineales Ax = b es triangular inferior si
-- todos los elementos de la matriz A que están por encima de la
-- diagonal principal son nulos; es decir, es de la forma
--    a(1,1)*x(1)                                               = b(1)
--    a(2,1)*x(1) + a(2,2)*x(2)                                 = b(2)
--    a(3,1)*x(1) + a(3,2)*x(2) + a(3,3)*x(3)                   = b(3)
--    ...
--    a(n,1)*x(1) + a(n,2)*x(2) + a(n,3)*x(3) +...+ a(x,x)*x(n) = b(n)
--
-- El sistema es compatible si, y sólo si, el producto de los elementos
-- de la diagonal principal es distinto de cero. En este caso, la
-- solución se puede calcular mediante el algoritmo de bajada:
--    x(1) = b(1) / a(1,1)
--    x(2) = (b(2) - a(2,1)*x(1)) / a(2,2)
--    x(3) = (b(3) - a(3,1)*x(1) - a(3,2)*x(2)) / a(3,3)
--    ...
--    x(n) = (b(n) - a(n,1)*x(1) - a(n,2)*x(2) -...- a(n,n-1)*x(n-1)) / a(n,n)
--
-- Definir la función
--    bajada :: Matrix Double -> Matrix Double -> Matrix Double
-- tal que (bajada a b) es la solución, mediante el algoritmo de bajada,
-- del sistema compatible triangular superior ax = b. Por ejemplo,
--    λ> let a = fromLists [[2,0,0],[3,1,0],[4,2,5.0]]
--    λ> let b = fromLists [[3],[6.5],[10]]
--    λ> bajada a b
--    ( 1.5 )
--    ( 2.0 )
--    ( 0.0 )
-- Es decir, la solución del sistema
--    2x            = 3
--    3x + y        = 6.5
--    4x + 2y + 5 z = 10
-- es x=1.5, y=2 y z=0.
-- ---------------------------------------------------------------------

module Algoritmo_de_bajada where

import Data.Matrix (Matrix, (!), fromLists, nrows, toLists)
import Test.Hspec (Spec, hspec, it, shouldBe)

bajada :: Matrix Double -> Matrix Double -> Matrix Double
bajada a b = fromLists [[x i] | i <- [1..m]]
  where m   = nrows a
        x k = (b!(k,1) - sum [a!(k,j) * x j | j <- [1..k-1]]) / a!(k,k)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "ej1" $
    toLists (bajada a b) `shouldBe` [[1.5],[2.0],[0.0]]
    where
      a = fromLists [[2,0,0],[3,1,0],[4,2,5.0]]
      b = fromLists [[3],[6.5],[10]]

-- La verificación es
--    λ> verifica
--
--    Finished in 0.0007 seconds
--    1 example, 0 failures
