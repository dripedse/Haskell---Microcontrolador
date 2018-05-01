import Test.Hspec
import MicroEntrega1

divisionPorCero = ejecutarMuchas [divide, lod 1, swap, lod 2, str 2 0, str 1 2]
division12Por4 = ejecutarMuchas [divide, lod 1, swap, lod 2, str 2 4, str 1 12]

runTests = hspec $ do
    describe "Test Punto 2 - Tests de NOP" $ do
        --it "NOP incrementa el program counter" $ do
         -- ((programCounter . ejecutar nop) xt8088) `shouldBe` 1
        
        it "NOP no cambia el acumulador" $ do
            ((acumuladorA . ejecutar nop) xt8088) `shouldBe` 0
      
        -- it "Programa con 3 NOP avanza 3 veces el program counter" $ do
          --  ((programCounter . ejecutarMuchas [nop, nop, nop]) xt8088) `shouldBe` 3
                
    describe "Test Punto 3 - Tests de programa Suma" $ do
        it "LODV de 5 lo carga en acumulador A" $ do
            ((acumuladorA . ejecutar (lodv 5)) xt8088) `shouldBe` 5
        
        it "SWAP cambia los valores de ambos acumuladores" $ do
            ((acumuladorA . ejecutar swap) fp20) `shouldBe` 24
    
        it "SWAP cambia los valores de ambos acumuladores (acumulador A)" $ do
            ((acumuladorB . ejecutar swap) fp20) `shouldBe` 7

        it "SWAP cambia los valores de ambos acumuladores (acumulador B)" $ do
            ((acumuladorA . ejecutar swap) fp20) `shouldBe` 24

        it "Suma 10 + 22 da 32 en Acumulador A" $ do
            ((acumuladorA . ejecutarMuchas [add, lodv 22, swap, lodv 10]) at8086) `shouldBe` 32

        it "Suma 10 + 22 da 0 en Acumulador B" $ do
            ((acumuladorB . ejecutarMuchas [add, lodv 22, swap, lodv 10]) at8086) `shouldBe` 0

        it "Suma 10 + 22 deja 5 en Program Counter" $ do
            ((programCounter . ejecutarMuchas [add, lodv 22, swap, lodv 10]) at8086) `shouldBe` 4

    describe "Test Punto 4 - Tests de programa División" $ do
        it "STR 2 5 para la memoria" $ do
            ((memoria . ejecutar (str 2 5)) at8086) `shouldBe` [1, 5, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

        it "LOD 2 de una memoria vacía debe dar 0" $ do
            (acumuladorA . ejecutar (lod 2) $ xt8088) `shouldBe` 0

        it "Division por cero da error" $ do
            (mensajeError $ divisionPorCero xt8088) `shouldBe` "DIVISION BY ZERO"
    
        it "Division por cero aumenta program counter" $ do
            (programCounter $ divisionPorCero xt8088) `shouldBe` 6
            
        it "Division de 12 por 4 se resuelve bien en Acumulador A" $ do
            (acumuladorA $ division12Por4 xt8088) `shouldBe` 3

        it "Division de 12 por 4 blanquea Acumulador B" $ do
            (acumuladorB $ division12Por4 xt8088) `shouldBe` 0

        it "Division de 12 por 4 no deja el mensaje de error porque funciona bien" $ do
            (mensajeError $ division12Por4 xt8088) `shouldBe` ""
        