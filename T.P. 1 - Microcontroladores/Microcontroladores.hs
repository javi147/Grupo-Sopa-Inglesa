import Text.Show.Functions
import Data.List

data Micro = Micro{
    memoria :: [Int],
    a :: Int,
    b :: Int,
    pc :: Int,
    etiqueta :: String,
    program :: [Instruccion]
} deriving (Show)

type Instruccion = Micro -> Micro
  
addCounter :: Instruccion
addCounter (Micro m a b pc "" i) = (Micro m a b (pc+1) "" i)
addCounter micro = micro

nop :: Instruccion
nop = addCounter

add :: Instruccion
add micro =  addCounter micro {
  a = a micro + b micro,
  b = 0
}
  
swap :: Instruccion  
swap micro = addCounter micro{
  a = b micro,
  b = a micro
}

lodv :: Int -> Instruccion 
lodv val micro = addCounter micro {
  a = val
}

divide :: Instruccion
divide micro
  | divisorEsCero micro = addCounter micro {
      b = 0,
    etiqueta = "DIVISION BY ZERO"
    }
  | otherwise = addCounter micro {
      a = a micro `div` b micro,
      b = 0
    }
  
divisorEsCero :: Micro -> Bool 
divisorEsCero micro = b micro == 0
  
lod :: Int -> Instruccion  
lod addr micro 
  | emptyMemo micro = micro
  | otherwise = addCounter micro {
    a = (!!) (memoria micro) (addr - 1)
    }

str :: Int -> Int -> Instruccion
str addr val micro 
  | emptyMemo micro = addCounter micro {
      memoria = (take (addr - 1) [0,0..]) ++ [val]
      }
  | addr <= ((length.memoria) micro) = addCounter micro {
      memoria = (take (addr - 1) (memoria micro)) ++ [val] ++ (drop (addr) (memoria micro))
      }
  | otherwise = addCounter micro {
      memoria = (memoria micro) ++ (take (addr - ((length.memoria) micro) - 1) [0,0..]) ++ [val]
      }

emptyMemo :: Micro -> Bool
emptyMemo = (==[]).memoria

strProgram :: Micro -> [Instruccion] -> Micro
strProgram micro program = micro {
  program = program
}

run :: Micro -> [Instruccion] -> Micro
run micro [] = micro
run micro (x:xs) = run (x micro) xs

runProgram :: Instruccion
runProgram (Micro m a b pc e []) = (Micro m a b pc e [])
runProgram micro = run ((head (program micro)) micro) (tail (program micro))


ifnz :: Micro -> [Instruccion] -> Micro
ifnz micro [] = micro
ifnz (Micro m 0 b pc e i) _ = (Micro m 0 b pc e i)
ifnz (Micro m a b pc "" i) (x:xs) = ifnz (x (Micro m a b pc "" i)) xs
ifnz micro _ = micro

debug :: [Instruccion] -> [Instruccion]
debug = filter (not.aBug)

aBug :: Instruccion -> Bool
aBug f = (==0).sum.(++ [a.f $ xt8088]).(++ [b.f $ xt8088]).memoria.f $ xt8088

memoriaOrdenada :: Micro -> Bool
memoriaOrdenada (Micro (x:xs) a b pc e p)
    | emptyMemo.memoria = true
    | xs < ultimo x = memoriaOrdenada (Micro x a b pc e p)
    | otherwise = false

ultimo (x:xs) = xs


xt8088= Micro [] 0 0 0 "" []

fp20 = Micro [] 7 24 0 "" []
  
at8086 = Micro [1..20] 0 0 0 "" []

add22to10 = reverse [add, lodv 22, swap, lodv 10]

divide2by0 = reverse [divide, lod 1, swap, lod 2, str 2 0,str 1 2]
