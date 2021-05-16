{-|
==Introdução
O objetivo desta tarefa era compactar um labirinto  .


==Conclusão/Estratégias
Começou-se por compactar  horizontalmente e posteriormente verticalmente (através do uso do “repeat “ ,que comparava a linha em que se encontra com as linhas anteriores ).
A compactação horizontal foi feita a partir de funções que contavam o número de elementos iguais na mesma linha . 


-}


module Tarefa3 where
import Types



-- | Função que agrupa elementos iguais na mesma linha do maze

groupLinha :: [Piece] -> [[Piece]]
groupLinha [] = []
groupLinha (h:t) = (h:takeWhile (== h) t) : (groupLinha (dropWhile (== h) t))


-- | Função que junta as várias peças de uma linha e devolve no estado (nº de vezes que ocorre,piece)

contagemGrupinho :: [Piece] -> [(Int,Piece)]
contagemGrupinho [] = []
contagemGrupinho c = [((length c),((!!) c 0))]


-- | Função que conta o nº de grupinhos numa linha

contagemLinha :: [[Piece]] -> [(Int,Piece)]
contagemLinha [] = []
contagemLinha (m:ms) = (contagemGrupinho m) ++ (contagemLinha ms)


-- | Função que aplica a função "contagemLinha" a todas as linhas do maze

contagemMaze :: [[Piece]] -> [[(Int,Piece)]]
contagemMaze [] = []
contagemMaze (m:ms) = (contagemLinha (groupLinha m)) : (contagemMaze ms) 


-- | Função que compara se dois corredores são iguais

comparaLista :: [Piece] -> [Piece] -> Bool
comparaLista [] _ = True
comparaLista _ []= False
comparaLista (h:t) (h':t') = h == h' && comparaLista t t' || comparaLista (h:t) t'


-- | Função que indica o indice da linha do maze ,onde o corredor é igual ao fornecido , primeira linha = indice 0, c = corredor, k = contador (começa em 0)

indicador :: Maze-> Corridor -> Int -> Int
indicador [] c k = 0
indicador (m:ms) c k | (c == m) = k 
                     | otherwise = indicador ms c (k+1)


-- | Funçao que compara o ultimo elemento da lista com os outros e devolve o repeat (indice da linha) ou a instruct ...

forLast :: Maze -> Int -> Instructions
forLast [m] k = [(Instruct (head(contagemMaze [m] )))]
forLast m k | ((elem (last m) (init m)) == False) = [(Instruct (head(contagemMaze [(last m)] )))]
            | otherwise = [(Repeat (indicador m (last m) k))]





-- | Funçao final

compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze (m1:ms) =(compactMaze (init (m1:ms))) ++ (forLast (m1:ms) 0) 










maze2 = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall]

    ]







maze1 = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty, Food Little, Wall, Empty,Empty, Empty, Empty, Wall, Food Little, Wall,Wall, Wall, Empty, Wall, Wall, Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty, Food Little, Wall, Empty,Empty, Empty, Empty, Wall, Food Little, Wall,Empty, Empty, Empty, Empty, Empty, Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Food Little,Wall, Wall, Wall, Wall, Wall, Wall,Food Little,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall, Wall]


     ]