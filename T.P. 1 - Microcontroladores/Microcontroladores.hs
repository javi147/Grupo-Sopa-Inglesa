import Text.Show.Functions
import Data.List
--Punto 1.1--
--Criterio utilizado para modelado del data se quiere manejar los campos con un constructor para mejor entendimiento y funcionamiento.--
data Microcontrolador = Microcontrolador{
	memoria :: [Int],
	acumuladorA :: Int,
	acumuladorB :: Int,
	programCounter :: Int,
	etiqueta :: String,
	instrucciones :: [Instruccion]
}deriving (Show)

type Instruccion= Microcontrolador -> Microcontrolador

--Punto 1.2--
xt8088= Microcontrolador [] 0 0 0 "" []

--Punto 2.1--
nop :: Microcontrolador -> Microcontrolador
nop unMicro = unMicro {
	programCounter = programCounter unMicro + 1
	}

saltarTresInstrucciones unMicro =
	(((nop).nop).nop) unMicro

--Interviene el concepto de composicion--

--Punto 3.1--
add :: Microcontrolador -> Microcontrolador
add unMicro =  nop unMicro{
	acumuladorA = acumuladorA unMicro + acumuladorB unMicro,
	acumuladorB = 0
	}
	
swap :: Microcontrolador -> Microcontrolador	
swap unMicro = nop unMicro{
	acumuladorA = acumuladorB unMicro,
	acumuladorB = acumuladorA unMicro
	}

lovd :: Int -> Microcontrolador -> Microcontrolador	
lovd val unMicro = nop unMicro {
	acumuladorA = val
	}

--Punto 3.2--
sumarDiesMasVeintidos :: Microcontrolador -> Microcontrolador
sumarDiesMasVeintidos unMicro =
	((((add).lovd 22).swap).lovd 10) unMicro

--Punto 4.1--
divide :: Microcontrolador -> Microcontrolador
divide unMicro
	| esCeroElDivisor unMicro = nop unMicro {
		acumuladorB = 0,
		etiqueta = "DIVISION BY ZERO"
		}
	| otherwise = nop unMicro {
		acumuladorA = acumuladorA unMicro `div` acumuladorB unMicro,
		acumuladorB = 0
		}
	
esCeroElDivisor :: Microcontrolador -> Bool	
esCeroElDivisor unMicro = (acumuladorB unMicro) == 0
	
lod :: Int -> Microcontrolador -> Microcontrolador	
lod addr unMicro= nop unMicro{
	acumuladorA = (!!) (memoria unMicro) addr
	}

str addr val (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) =
	Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones 
	
--Punto 4.2--
divisionDosPorCero :: Microcontrolador -> Microcontrolador
divisionDosPorCero unMicro=
	((((((divide).lod 1).swap).lod 2).str 2 0).str 1 2) unMicro

											--CASOS DE PRUEBA--
--Punto 2--

--saltarTresInstrucciones xt8088

--Punto 3--

--lodv 5 xt8088

fp20 = Microcontrolador [] 7 24 0 "" []
--swap fp20

--sumarDiesMasVeinte xt8088

--Punto 4--

at8086 = Microcontrolador [1..20] 0 0 0 "" []

--str 2 5 at8086

--xt8088 = Microcontrolador (replicate 1024 0) 1 0 0 "" []
--lod 2 xt8088

--divisionDosPorCero xt8088
	
divisionDocePorCuatro unMicro=
	((((((divide).lod 1).swap).lod 2).str 2 4).str 1 12) unMicro
