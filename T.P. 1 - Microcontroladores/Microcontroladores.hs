  import Text.Show.Functions
  import Data.List

data Microcontrolador = Microcontrolador {
  memoria :: [Int],
  a :: Int,
  b :: Int,
  programCounter :: Int,
  etiqueta :: String
}deriving (Show)

nop :: Microcontrolador -> Microcontrolador
nop unMicro = unMicro {
  programCounter = programCounter unMicro + 1
}

add :: Microcontrolador -> Microcontrolador
add unMicro =  nop unMicro {
  a = a unMicro + b unMicro,
  b = 0
}
  
swap :: Microcontrolador -> Microcontrolador  
swap unMicro = nop unMicro{
  a = b unMicro,
  b = a unMicro
}

lovd :: Int -> Microcontrolador -> Microcontrolador 
lovd val unMicro = nop unMicro {
  a = val
}

divide :: Microcontrolador -> Microcontrolador
divide unMicro
  | esCeroElDivisor unMicro = nop unMicro {
    b = 0,
    etiqueta = "DIVISION BY ZERO"
    }
  | otherwise = nop unMicro {
    a = a unMicro `div` b unMicro,
    b = 0
    }
  
esCeroElDivisor :: Microcontrolador -> Bool 
esCeroElDivisor unMicro = (b unMicro) == 0
  
lod :: Int -> Microcontrolador -> Microcontrolador  
lod addr unMicro= nop unMicro{
  a = (!!) (memoria unMicro) (addr - 1)
  }

str :: Int -> Int -> Microcontrolador -> Microcontrolador 
str addr val unMicro = nop unMicro{
  memoria = take (addr - 1)(memoria unMicro) ++ [val] ++ drop (addr) (memoria unMicro)
  }

xt8088= Microcontrolador [] 0 0 0 ""

fp20 = Microcontrolador [] 7 24 0 ""

at8086 = Microcontrolador [1..20] 0 0 0 ""