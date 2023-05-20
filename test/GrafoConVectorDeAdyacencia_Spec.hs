module GrafoConVectorDeAdyacencia_Spec (main, spec) where

import TAD.GrafoConVectorDeAdyacencia
import Test.Hspec

main :: IO ()
main = hspec spec

ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

spec :: Spec
spec = do
  it "1" $
    show (creaGrafo D (1,3) [(1,2,0),(3,2,0),(2,2,0)]) `shouldBe`
    "G D ([1,2,3],[(1,2),(2,2),(3,2)])"
  it "2" $
    show (creaGrafo ND (1,3) [(1,2,0),(3,2,0),(2,2,0)]) `shouldBe`
    "G ND ([1,2,3],[(1,2),(2,2),(2,3)])"
  it "3" $
    show (creaGrafo ND (1,3) [(1,2,0),(3,2,5),(2,2,0)]) `shouldBe`
    "G ND ([1,2,3],[((1,2),0),((2,2),0),((2,3),5)])"
  it "3" $
    show (creaGrafo D (1,3) [(1,2,0),(3,2,5),(2,2,0)]) `shouldBe`
    "G D ([1,2,3],[((1,2),0),((2,2),0),((3,2),5)])"
  it "e4" $
    show ejGrafoND `shouldBe`
    "G ND ([1,2,3,4,5],[((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),((3,4),61),((3,5),44),((4,5),93)])"
  it "e5" $
    show ejGrafoD `shouldBe`
    "G D ([1,2,3,4,5],[((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),((3,4),61),((3,5),44),((4,5),93)])"
  it "e6" $
    dirigido ejGrafoD   `shouldBe`  True
  it "e7" $
    dirigido ejGrafoND  `shouldBe`  False
  it "e8" $
    nodos ejGrafoND  `shouldBe`  [1,2,3,4,5]
  it "e9" $
    nodos ejGrafoD   `shouldBe`  [1,2,3,4,5]
  it "e10" $
    adyacentes ejGrafoND 4  `shouldBe`  [2,3,5]
  it "e11" $
    adyacentes ejGrafoD  4  `shouldBe`  [5]
  it "e12" $
    aristaEn ejGrafoND (5,1)  `shouldBe`  True
  it "e13" $
    aristaEn ejGrafoND (4,1)  `shouldBe`  False
  it "e14" $
    aristaEn ejGrafoD  (5,1)  `shouldBe`  False
  it "e15" $
    aristaEn ejGrafoD  (1,5)  `shouldBe`  True
  it "e16" $
    peso 1 5 ejGrafoND  `shouldBe`  78
  it "e17" $
    peso 1 5 ejGrafoD   `shouldBe`  78
  it "e18" $
    aristas ejGrafoD `shouldBe`
    [((1,2),12),((1,3),34),((1,5),78),
     ((2,4),55),((2,5),32),
     ((3,4),61),((3,5),44),
     ((4,5),93)]
  it "e19" $
    aristas ejGrafoND `shouldBe`
    [((1,2),12),((1,3),34),((1,5),78),
     ((2,1),12),((2,4),55),((2,5),32),
     ((3,1),34),((3,4),61),((3,5),44),
     ((4,2),55),((4,3),61),((4,5),93),
     ((5,1),78),((5,2),32),((5,3),44),((5,4),93)]
