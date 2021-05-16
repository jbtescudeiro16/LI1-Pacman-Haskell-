module Genesrator where

import System.Random

type Labirinto = [Corredor]
type Corredor = [Peca]
data Peca = Comida TamanhoComida | Parede | Chao
data TamanhoComida = Grande | Pequena

instance Show Peca where
    show (Comida Grande) = "o"
    show (Comida Pequena) = "."
    show (Parede) = "#"
    show (Chao) = " "

sampleMaze :: Labirinto
sampleMaze = [
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede],
                [Chao, Comida Pequena, Comida Pequena, Comida Grande, Comida Pequena, Comida Grande, Comida Pequena, Chao],
                [Parede, Parede, Parede, Parede, Parede, Parede, Parede, Parede]
            ]


-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- Converssta list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converts an integer number into a Peca
-- 3 <=> Comida Granfre
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 9 <=> Parede
--
convertePeca :: Int -> Peca
convertePeca x | x==3 = Comida Grande
               | x<70  = Comida Pequena
               | x<=90 = Parede
               | otherwise = Chao

printCorridor :: Corredor -> String
printCorridor [] = "\n"
printCorridor (h:t) = show h ++ printCorridor t


-- | Converts a Labirinto to a string
--
printMaze :: Labirinto -> String
printMaze [] = ""
printMaze (x:xs) = printCorridor x ++ printMaze xs

-- | Converts a list of integers into a Corredor
--
converteCorredor :: [Int] -> Corredor
converteCorredor [] = []
converteCorredor (h:t)= convertePeca h : converteCorredor t 


-- | Converts a list of lists of integers into a Labirinto
--
converteLabirinto :: [[Int]]-> Labirinto
converteLabirinto [] = []
converteLabirinto (h:t) = converteCorredor h : converteLabirinto t



geraLabirinto :: Int -> Int -> Int -> IO ()
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in putStrLn $ printMaze $ converteLabirinto $ subLista x random_nrs




--generateMaze :: Int -> Int -> Int -> Maze


--gerar aleatorios para serem chamados na função geracorredores

--calcular o n de celulas para as quais se tem gerar nrs aleatorios
calcularcelulas :: Int-> Int ->Int
calcularcelulas h c =(h-2)*(c-2)

--  gera o total de nrs para o labirinto todo exceto as paredes 
-- s = seed introduzida
gerarali :: Int -> Int -> Int -> [Int]
gerarali h c s  = geraAleatorios (calcularcelulas h c) s





--faz paredes de tamanho definido !!PAREDES!!
geraParedes:: Int -> Corredor
geraParedes 0 = []
geraParedes x = Parede : geraParedes (x-1) 


--h =altura , c=comprimento
--  dados os aleatórios converte-os num corredor válido

geraCorredoresMeio :: [Int] -> Corredor
geraCorredoresMeio [] = []
geraCorredoresMeio l = [Parede] ++ converteCorredor l ++ [Parede]


--n=comprimento interior sem contar com as paredes :total=(n+2)
-- introduzindo o n , "separa" os elementos da lista x em listas de pecas com n elemntos

geranCorredores :: Int -> [Int] -> Labirinto  
geranCorredores _ [] = []
geranCorredores n x = geraCorredoresMeio (take n x) : geranCorredores n (drop n x)


-- Gera o interior com valores random

chamarcorredores :: Int -> Int -> Int-> Labirinto
chamarcorredores h c s =  [geraParedes c] ++ geranCorredores (c-2) ( gerarali h c s) ++ [geraParedes c]



--calcula quantas celulas temos de imprimir sem os limites do labirinto

--calcularcelulastunel :: Int -> (Int,Int)
--calcularcelulastunel h =if  (even h) == True then  show (div h 2)



  

--gera uma linha com x elementos do tipo ( Chao )

geraChao :: Int -> Corredor
geraChao 0 = []
geraChao y = Chao : geraChao (y-1)   





--a casa possui 3 + 2 de altura , contando com a linha vazia 
-- seja c o comprimento , de um corredor, com c par

--imprime1Casa :: Int -> [Int] ->  Corredor
--imprime1Casa [] = []
--imprime1casa c x= take (div ( c-8 2 ) x ++ geraChao 8 ++ drop ((div c-8 2) +8) xs