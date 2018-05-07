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

type Instruccion= Microcontrolador->Microcontrolador
	
--Punto 1.2--
xt8088= Microcontrolador [] 0 0 0 "" [nop, nop, nop]

--Punto 2.1--
nop :: Microcontrolador -> Microcontrolador
nop (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) =
	aumentarProgramCounter (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta (tail instrucciones))
--Punto 2.2--
saltarTresIntrucciones :: Microcontrolador -> Microcontrolador
saltarTresIntrucciones microcontrolador= (nop.(nop.(nop)))microcontrolador
--Conviene usar el consepto de composicion--

--Punto 3.1--
aumentarProgramCounter :: Microcontrolador -> Microcontrolador
aumentarProgramCounter (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) =
	Microcontrolador memoria acumuladorA acumuladorB ((+) 1 programCounter) etiqueta instrucciones

modificarMicro :: (Int -> Int -> Int) ->Microcontrolador -> Microcontrolador		
modificarMicro (/) (Microcontrolador memoria 2 0 programCounter etiqueta instrucciones) =
	Microcontrolador memoria 2 0 programCounter "DIVISION BY ZERO" instrucciones
modificarMicro funcion (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) = 
	Microcontrolador memoria (funcion acumuladorA acumuladorB) 0 programCounter etiqueta instrucciones
	
add :: Microcontrolador -> Microcontrolador 		
add microcontrolador =  
	(aumentarProgramCounter.(modificarMicro (+)))microcontrolador
    
swap :: Microcontrolador -> Microcontrolador
swap microcontrolador =
	(aumentarProgramCounter.(cambiarAcumuladores))microcontrolador
	
cambiarAcumuladores :: Microcontrolador -> Microcontrolador	
cambiarAcumuladores (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones)=
	Microcontrolador memoria acumuladorB acumuladorA programCounter etiqueta instrucciones
	
lodv :: Int -> Microcontrolador -> Microcontrolador
lodv val microcontrolador =
	(aumentarProgramCounter.(cambiarAcumuladorA val))microcontrolador
	
cambiarAcumuladorA :: Int -> Microcontrolador -> Microcontrolador	
cambiarAcumuladorA valor (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) =
	Microcontrolador memoria valor acumuladorB programCounter etiqueta instrucciones
	
--Punto 3.2--
sumarDiezMasVeinte :: Microcontrolador -> Microcontrolador
sumarDiezMasVeinte microcontrolador = 
	((((add).lodv 22).swap).lodv 10)microcontrolador

--Punto 4.1--
divide :: Microcontrolador -> Microcontrolador
divide microcontrolador = 
	(aumentarProgramCounter.(modificarMicro div))microcontrolador

lod :: Int -> Microcontrolador -> Microcontrolador
lod addr microcontrolador =	
	(aumentarProgramCounter.(cargarContenido addr))microcontrolador
	
cargarContenido :: Int -> Microcontrolador -> Microcontrolador 	
cargarContenido valor (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones) =
	Microcontrolador memoria ((!!) memoria valor) acumuladorB programCounter etiqueta instrucciones
	
str :: Int ->Int ->Microcontrolador-> Microcontrolador
str	addr val microcontrolador =
	(aumentarProgramCounter.(reemsamblarMemoria addr val)) microcontrolador

reemsamblarMemoria :: Int -> Int -> Microcontrolador-> Microcontrolador	
reemsamblarMemoria addr val (Microcontrolador memoria acumuladorA acumuladorB programCounter etiqueta instrucciones)=
	Microcontrolador ((++) (memoriaUno addr val memoria) (memoriaDos addr memoria)) acumuladorA acumuladorB programCounter etiqueta instrucciones
	
memoriaUno :: Int-> Int ->  [Int]-> [Int] 	
memoriaUno addr val memoria =
	(reverse.(:) val.(take (addr-1).(init.(fst.(splitAt ((!!) memoria addr)))))) memoria
	
memoriaDos :: Int ->[Int]-> [Int]
memoriaDos addr memoria=
	(snd.(splitAt ((!!) memoria addr - 1))) memoria
	
	
--4 Casos de Prueba--

--4.2 Luego de avanzar el procesador xT8088 tres veces, se espera que el program
--counter quede en 3. Los acumuladores deben quedar en cero, con la memoria
--vacía y sin etiqueta de errores.

	--saltarTresIntrucciones xT8088

--4.3 LODV 5 tiene:
-- ○ como precondiciones: el acumulador A y B están en cero
-- ○ como post-condiciones: el acumulador A tiene valor 5 y el B cero.

	--xt8088= Microcontrolador [] 0 0 0 "" []
	--lodv 5 xT8088	
	
--"Dado un procesador fp20 que tiene acumulador A con 7 y acumulador B con
--24, al ejecutar SWAP el acumulador A debe quedar con 24 y el B con 7."
	
fp20 = Microcontrolador [] 7 24 0 "" []
	--swap fp20

--"Luego de ejecutar el programa que suma 10 + 22, el acumulador A debe
--quedar en 32 y el B en 0."
	
	--sumarDiezMasVeinte fp20
	
--4.4 
--"Dado el procesador at8086 que tiene los acumuladores en cero, el program
--counter en 0, sin mensaje de error y una memoria con los siguientes datos:
--[1..20], le ejecutamos la instrucción STR 2 5. Entonces el procesador at8086
--debe quedar con un 5 en la posición 2: [1, 5, 3, 4, 5,...]"
	
at8086 = Microcontrolador [1..20] 0 0 0 "" []
--at8086 str 2 5


--"LOD 2 de un procesador xt8088 con la memoria vacía (1024 posiciones con
--valores cero2) debe dejar con cero el acumulador A (cero = ausencia de
--información)"

--xt8088 = Microcontrolador (replicate 1024 0) 5 100 0 "" []
--lod 2 xt8088
		
--"Ejecutar por consola la división 2 por 0 para el procesador xt8088 según el
--"Ejecutar por consola la división 2 por 0 para el procesador xt8088 según el
--programa escrito arriba, esperamos el mensaje de error “DIVISION BY ZERO”,
--y un 6 en el program counter."

--xt8088 = Microcontrolador [] 2 0 0 "" []
--divide xt8088


--"Ejecutar la división de 12 por 4 para el procesador xt8088 (cambiando los
--valores del programa anterior), que debe dar 3 y no tirar ningún mensaje de error"	

--xt8088 = Microcontrolador [] 12 4 0 "" []
--divide xt8088