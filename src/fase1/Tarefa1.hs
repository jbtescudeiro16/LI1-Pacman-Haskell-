{- | 
==Intrdução / Objetivos
Face ao grande ojetivo desta tarefa de construir um mapa para o pacman , utilizaram-se estratégias que permitissem gerar labirintos válidos de acordo com o enunciado

== Conclusão /Análise
Assim ,após a realização de diversas funções para que o mesmo funcionasse, os maiores desafios foram a implementação dos túneis , bem como a casa  dos fantasmas . 
Estes foram superados com sucesso , e o objetivo desta tarefa foi concluído .
 
-}

module Tarefa1 where 

import System.Random
import Types


-- | gera um dado numero de aleatórios

generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed 
                        in take n $ randomRs (0,99) gen


-- | dado um número converte-o numa peça

convertePeca :: Int -> Piece
convertePeca x | x==3 = Food Big
               | x<70  = Food Little
               | x<=90 = Wall
               | otherwise = Empty

               
-- | converte um corredor por uma string com \n no final 

printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (h:t) = show h ++ printCorridor t


-- | Converte uma lista de inteiros num corredor 

converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (h:t)= convertePeca h : converteCorredor t 


-- | Converte uma lista de corredores num labirinto

converteLabirinto :: [[Int]]-> Maze
converteLabirinto [] = []
converteLabirinto (h:t) = converteCorredor h : converteLabirinto t


-- | calcular o n de celulas para as quais se tem gerar nº aleatorios

calcularcelulas :: Int-> Int ->Int
calcularcelulas c h  =(c-2)*(h-2)


-- | gera o total de nº para o labirinto todo exceto as paredes 
-- | s = seed introduzida

gerarali :: Int -> Int -> Int -> [Int]
gerarali 0 0 _ = []
gerarali c h s  = generateRandoms (calcularcelulas c h) s


-- | gera corredor só com peças do tipo (Empty) de tamanho definido 

geraChao :: Int -> Corridor
geraChao 0 = []
geraChao y = Empty : geraChao (y-1)   


-- | gera corredor só com peças do tipo (Wall) de tamanho definido (Int)

geraParedes:: Int -> Corridor
geraParedes 0 = []
geraParedes x = Wall : geraParedes (x-1) 


-- | dada uma lista de números aleatórios converte-os num corredor válido , com paredes nos extremos

geraCorredoresMeio :: [Int] -> Corridor
geraCorredoresMeio [] = []
geraCorredoresMeio l = [Wall] ++ converteCorredor l ++ [Wall]


-- | n=comprimento interior sem contar com as paredes :total=(n+2)
-- | introduzindo o n , "separa" os elementos da lista x em listas de pecas com n elemntos

geranCorredores :: Int -> [Int] -> Maze  
geranCorredores _ [] = []
geranCorredores n x = geraCorredoresMeio (take n x) : geranCorredores n (drop n x)


-- | Gera o interior com valores random

chamarcorredores :: Int -> Int -> Int-> Maze
chamarcorredores c h s =  [geraParedes c] ++ geranCorredores (c-2) ( gerarali c h s) ++ [geraParedes c]


-- | calcula o meio de um labirinto em altura

calcularmeio:: Maze -> Int
calcularmeio l = (div (length l) 2)


-- | meio= meio do labirinto ,t= contador (diz a linha para ser alterada) onde serão colocados os túneis, h= altura do maze  , lista l= labirinto

injetaTuneis :: Int -> Int -> Int-> Maze -> Maze
injetaTuneis meio t h [l] = [l]
injetaTuneis meio t h (l1:l2:ls) 
                            | even h && meio == t+1 = substituiEmpty l1: substituiEmpty l2 : ls
                            | meio == t = substituiEmpty l1 : (l2:ls )
                            | otherwise = l1 : injetaTuneis meio (t+1) h (l2:ls)


-- | substitui o 1º e o ultimo elemento de um corredor por peças do tipo (Empty),que será usado ao gerar os túneis

substituiEmpty :: Corridor -> Corridor
substituiEmpty [] = []
substituiEmpty (c:cs) = Empty : take ( length cs -1 ) cs ++ [Empty]        


-- | funçao que imprime tuneis nas posiçoes centrais

n::Maze ->Maze
n h = injetaTuneis ( calcularmeio h) 0 (length h) h

-- | Supondo que a casa dos fantasmas tem 5 de altura contando com os corredores (Empty)
-- | dado um corredor e substitui parte dele pela 1º\5º linha da casa dos fantasmas 

homeLine1_5 :: Corridor -> Corridor  
homeLine1_5 [] = []                              
homeLine1_5 l | even (length l) = (take ( procurarInicio (length l) ) l) ++ geraChao 10 ++ drop ( (procurarInicio (length l) )+10) l
              | otherwise = (take ( procurarInicio (length l) ) l) ++ geraChao 11 ++ drop ( (procurarInicio (length l) )+11) l


-- | dado um corredor substitui parte dele pela 2º linha da casa dos fantasmas 
 
homeline2 :: Corridor -> Corridor
homeline2 [] = []
homeline2 l | even (length l) = (take (procurarInicio (length l) ) l ) ++ geraChao 1 ++ geraParedes 3 ++ geraChao 2 ++ geraParedes 3 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+10) l
            | otherwise = (take (procurarInicio (length l) ) l) ++ geraChao 1 ++ geraParedes 3 ++geraChao 3 ++ geraParedes 3 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+11) l


-- | dado um corredor substitui parte dele pela 3º linha da casa dos fantasmas 

homeline3 :: Corridor -> Corridor
homeline3 [] = []
homeline3 l | even (length l) = (take ( procurarInicio (length l) ) l) ++ geraChao 1 ++ geraParedes 1 ++ geraChao 6 ++ geraParedes 1 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+10) l
            | otherwise = (take ( procurarInicio (length l) ) l) ++ geraChao 1 ++ geraParedes 1 ++ geraChao 7 ++ geraParedes 1 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+11) l


-- | dado um corredor substitui parte dele pela 4º linha da casa dos fantasmas 

homeline4 :: Corridor -> Corridor
homeline4 [] = []
homeline4 l | even (length l) = (take ( procurarInicio (length l) ) l) ++ geraChao 1 ++ geraParedes 8 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+10) l
            | otherwise = (take ( procurarInicio (length l) ) l) ++ geraChao 1 ++ geraParedes 9 ++ geraChao 1 ++ drop ( (procurarInicio (length l) )+11) l


-- | procura a posiçao inicial da casa contado com os espaços vazios que a envolvem

procurarInicio :: Int -> Int
procurarInicio c | even c = (div c 2) - 5   
                 | otherwise = ((div c 2) +1) -6


-- | h altura , meio=meio do labirinto em altura ,t =contador

substituiForLine1 :: Int -> Int -> Int-> Maze -> Maze
substituiForLine1 meio t h [l] = [l]
substituiForLine1 meio t h (l:ls) 
                               | (even h) && (meio-3) == t = homeLine1_5 l : substituiForLine1 meio (t+1) h ls 
                               | (even h) && (meio-2) == t = homeline2 l : substituiForLine1 meio (t+1) h ls --(quando a o altura do labirinto for par,linha2)
                               | (even h) && (meio-1) == t = homeline3 l : substituiForLine1 meio (t+1) h ls
                               | (even h) && meio == t = homeline4 l : substituiForLine1 meio (t+1) h ls
                               | (even h) && (meio+1) == t = homeLine1_5 l : substituiForLine1 meio (t+1) h ls
                               | (odd h) && (meio-2) == t  = homeLine1_5 l : substituiForLine1 meio(t+1) h ls 
                               | (odd h) && (meio-1) == t = homeline2 l : substituiForLine1 meio (t+1) h ls
                               | (odd h) && meio == t = homeline3 l : substituiForLine1 meio (t+1) h ls
                               | (odd h) && (meio+1) == t = homeline4 l : substituiForLine1 meio (t+1) h ls
                               | (odd h) && (meio+2) == t = homeLine1_5 l :substituiForLine1 meio (t+1) h ls 
                               | otherwise = l : substituiForLine1 meio (t+1) h ls 


-- | Funçao final que chama todas as funçoes necessarias para gerar o labirinto na forma final desejada

generateMaze :: Int -> Int -> Int -> Maze
generateMaze c h s | even c && even h = n(substituiForLine1 (calcularmeio (chamarcorredores c h s)) 0 h (chamarcorredores c h s))
                   | even c && odd h = n(substituiForLine1 (calcularmeio (chamarcorredores c h s))  0  h (chamarcorredores c h s))
                   | odd c && even h = n(substituiForLine1 (calcularmeio (chamarcorredores c h s)) 0 h (chamarcorredores c h s))
                   | otherwise = n(substituiForLine1 (calcularmeio (chamarcorredores c h s)) 0 h (chamarcorredores c h s))
