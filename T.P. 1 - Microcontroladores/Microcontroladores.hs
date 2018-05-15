import Text.Show.Functions
import Data.List

data Micro = Micro{
    memory :: [Int],
    a :: Int,
    b :: Int,
    pc :: Int,
    errorMsg :: String,
    program :: [Instruction]
} deriving (Show)

type Instruction = Micro -> Micro
  
addCounter :: Instruction
addCounter (Micro m a b pc "" p) = (Micro m a b (pc+1) "" p)
addCounter micro = micro

setA :: Int -> Instruction
setA valor micro = micro {
  a = valor
}

setB :: Int -> Instruction
setB valor micro = micro {
  b = valor
}

setMemo :: [Int] -> Instruction
setMemo list micro = micro {
  memory = list
}

nop :: Instruction
nop = addCounter

add :: Instruction
add micro = addCounter.setB 0.setA (a micro + b micro) $ micro
  
swap :: Instruction   
swap micro = addCounter.setB (a micro).setA (b micro) $ micro

lodv :: Int -> Instruction 
lodv valor = addCounter.setA valor

divide :: Instruction
divide (Micro m a 0 pc e p) = addCounter (Micro m a 0 pc "DIVISION BY ZERO" p ) 
divide micro = addCounter.setB 0.(setA (a micro `div` b micro)) $ micro
  
lod :: Int -> Instruction  
lod addr micro 
  | emptyMemo micro = micro
  | otherwise = addCounter.setA (memory micro !! (addr - 1)) $ micro

str :: Int -> Int -> Instruction
str addr val micro 
  | emptyMemo micro = addCounter.setMemo ((take (addr - 1) [0,0..]) ++ [val]) $ micro
  | addr <= ((length.memory) micro) = 
    addCounter.setMemo ((take (addr - 1) (memory micro)) ++ [val] ++ (drop (addr) (memory micro))) $ micro
  | otherwise = 
    addCounter.setMemo ((memory micro) ++ (take (addr - ((length.memory) micro) - 1) [0,0..]) ++ [val]) $micro

emptyMemo :: Micro -> Bool
emptyMemo = (==[]).memory

strProgram :: Micro -> [Instruction] -> Micro
strProgram micro program = micro {
  program = reverse program
}

run :: Micro -> [Instruction] -> Micro
run (Micro m a b pc "" p) [] = (Micro m a b pc "" p)
run (Micro m a b pc "" p) (x:xs) = run (x (Micro m a b pc "" p)) xs
run micro _ = micro

runProgram :: Instruction
runProgram (Micro m a b pc e []) = (Micro m a b pc e [])
runProgram micro = run ((head (program micro)) micro) (tail (program micro))

ifnz :: Micro -> [Instruction] -> Micro
ifnz micro [] = micro
ifnz (Micro m 0 b pc e i) _ = (Micro m 0 b pc e i)
ifnz (Micro m a b pc "" i) (x:xs) = ifnz (x (Micro m a b pc "" i)) xs
ifnz micro _ = micro

debug :: [Instruction] -> [Instruction]
debug = filter (not.aBug)

aBug :: Instruction -> Bool
aBug f = (==0).sum.(++ [a.f $ xt8088]).(++ [b.f $ xt8088]).memory.f $ xt8088

orderedMemo :: Micro -> Bool
orderedMemo (Micro [] _ _ _ _ _) = True
orderedMemo (Micro (x:xs) _ _ _ _ _) = (all (>= x) xs) && orderedMemo (Micro xs 0 0 0 "" [])

xt8088= Micro [] 0 0 0 "" []

fp20 = Micro [] 7 24 0 "" []

at8086 = Micro [1..20] 0 0 0 "" []

mi8088 = Micro [0,0..] 0 0 0 "" []

-- Si intentamos cargar y ejecutar cualquier programa al micro
-- con memory infinita nunca se llegara a cargar porque
-- strProgram devuelve un dato de tipo Micro, cuya
-- primer variable devuelve una lista infinita y esta nunca
-- termina de resolverse.

-- Si intentamos aplicar orderedMemo a el micro con
-- memory infinita, nunca terminara de comparar si su primer
-- elemento es menor o igual a los subsiguientes, porque 
-- los subsiguientes son infinitos.

-- El constructor Micro fue modelado para representar un
-- microcontrolador, una computadora para aplicaciones
-- especificas. Como toda computadora su memory es
-- limitada, por lo tanto definir un microcontrolador
-- con memory infinita no tiene sentido.

add22to10 = [lodv 10, swap, lodv 22, add]

divide2by0 = [str 1 2, str 2 0, lod 2, swap, lod 1, divide]