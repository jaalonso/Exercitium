-- Caminos_en_un_triangulo.hs
-- Caminos en un triángulo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-abril-2024
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los triángulos se pueden representar mediante listas de listas. Por
-- ejemplo, el triángulo
--       3
--      7 4
--     2 4 6
--    8 5 9 3
-- se representa por
--    [[3],[7,4],[2,4,6],[8,5,9,3]]
--
-- Definir la función
--    caminos :: [[a]] -> [[a]]
-- tal que (caminos xss) es la lista de los caminos en el triángulo xss
-- donde los caminos comienzan en el elemento de la primera fila, en cada
-- paso se mueve a uno de  sus dos elementos adyacentes en la fila
-- siguiente y terminan en la última fila. Por ejemplo,
--    λ> caminos [[3],[7,4]]
--    [[3,7],[3,4]]
--    λ> caminos [[3],[7,4],[2,4,6]]
--    [[3,7,2],[3,7,4],[3,4,4],[3,4,6]]
--    λ> caminos [[3],[7,4],[2,4,6],[8,5,9,3]]
--    [[3,7,2,8],[3,7,2,5],[3,7,4,5],[3,7,4,9],[3,4,4,5],[3,4,4,9],[3,4,6,9],[3,4,6,3]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Caminos_en_un_triangulo where

import Test.Hspec (Spec, hspec, it, shouldBe)

caminos :: [[a]] -> [[a]]
caminos []    = [[]]
caminos [[x]] = [[x]]
caminos ([x]:[y1,y2]:zs) =
  [x:y1:us | (_:us) <- caminos ([y1] : map init zs)] ++
  [x:y2:vs | (_:vs) <- caminos ([y2] : map tail zs)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    caminos [[3],[7,4]] `shouldBe`
    [[3,7],[3,4]]
  it "e2" $
    caminos [[3],[7,4],[2,4,6]] `shouldBe`
    [[3,7,2],[3,7,4],[3,4,4],[3,4,6]]
  it "e3" $
    caminos [[3],[7,4],[2,4,6],[8,5,9,3]] `shouldBe`
    [[3,7,2,8],[3,7,2,5],[3,7,4,5],[3,7,4,9],[3,4,4,5],[3,4,4,9],[3,4,6,9],[3,4,6,3]]

-- La verificación es
--    λ> verifica
--
--    3 examples, 0 failures
