
{-|
==Introdução
*Nesta parte do trabalho o objetivo final foi implementar todo o tipo de jogadas do pacman , e posteriormente as dos ghosts para que as funções das tarefas 4 e 5 ffuncionássem de forma correta.

==Estratégias 
    *Optou -se por criar diversas funções auxiliares  que realizassem as ações no pacman e nos ghosts  , como por exemplo 
    
	1.Alterar estados 
	2.Alterar coordenadas , bem como o labirinto 
	3.lterar todo o tipo de pontuação no pacman ( pontos por ingerir comidas ,perder vida)

*Por fim  o grupo chamou todas estas funções na função principal “play” .

==Conclusão 

*A estrutura da tarefa 2 foi alterada após a primeira entrega do trabalho de modo a que as funções desta tarefa pudessem receber jogadas quer para os fantasmas quer para o pacman , de modo a que as funções das tarefas 4,5 e 6 consigam funcionar .Assim a função principal “play” chama duas funções auxiliares “playghost” e “ playpac” , que tal como o nome sugere aplicam jogadas  e as suas consequências ,respetivamente , aos fantasmas e ao pacman   . 

-}
module Tarefa2 where

import Types
import Tarefa1


-- | dada uma lista de jogadores e o id de um jogador devolve o jogador com o respetivo id

findPlayer :: [Player] -> Int -> Player 
findPlayer (h:t) x | getPlayerID h == x = h
                   | otherwise = findPlayer t x


-- | dada uma lista de jogadores um inteiro e um contador "0" dá a posição desse elemento na lista de jogadores

indicePlayer :: [Player] -> Int -> Int -> Int 
indicePlayer (h:t) x y | getPlayerID h == x = y
                       | otherwise = indicePlayer t x (y+1)

-- | função final alterada , que aplica jogadas tanto a fantasmas como a pacman   

play :: Play ->State ->State 
play move@(Move i o ) ste@(State m listajog n ) 
                                | testafantasma zeca  = playghost move ste
                                | otherwise = playpac move ste 
      where zeca = findPlayer listajog i


-- | Função que nos diz se o a jogada que estamos a aplicar é a um fantsma ou não  

testafantasma :: Player ->Bool
testafantasma (Ghost (GhoState (pid,cords,v,ori,points,lives) t )) = True
testafantasma _ = False

-- | função que decide se o fantasma pode jogar ou não mediante se existe ou não parede . 

playghost :: Play ->State ->State
playghost move@(Move i o ) (State m listajog n ) | encontra (novaCord (getPlayerCoords zeca) move m) m == Wall = (State m listajog n )
                                                 | otherwise = (State m b n )
                    where b = replaceNElem (indicePlayer listajog i 0) (novacoordenadapacman (findPlayer listajog i) move m) listajog
                          zeca= findPlayer listajog i


-- | função final que testa as diversas possibilidades como orientações , novas coordenadas , modo do pacman ,coordenadas coincidentes 

playpac :: Play -> State -> State
playpac move@(Move i o) (State m listajog n)     
                                               | (getPlayerOrientation zeca == o) && (checkCoords (newcord) listajog) == True = ghost move (newcord) (State m listajog n)                                                
                                               |  testaParede move (getPlayerCoords((findPlayer listajog i))) m == True && o==getPlayerOrientation (findPlayer listajog i) && getPacmanMode (findPlayer b i)== Mega = (State newm (alterastateGhost b) n)
                                               |  testaParede move (getPlayerCoords((findPlayer listajog i))) m == True && o==getPlayerOrientation (findPlayer listajog i) = (State newm b n) -- troca só a coordenada do pacman e no labririnto 
                                               |  o/=getPlayerOrientation (findPlayer listajog i) = (State m a n) --Só troca a orientção 
                                               | otherwise = (State m listajog n) 
            where a = (replaceNElem (indicePlayer listajog i 0) (coordenadaEtrocaOrientacao (findPlayer listajog i) move) listajog)
                  b = replaceNElem (indicePlayer listajog i 0) (novacoordenadapacman (findPlayer listajog i) move m) listajog
                  newm = pacToEmpty (findPlayer b i) move pacEmpty
                  pacEmpty = posPacEmpty ((getPlayerCoords((findPlayer listajog i)))) m 
                  zeca= findPlayer listajog i
                  newcord= (novaCord (getPlayerCoords zeca) move m)


-- | dado um jogador dá-nos o inteiro correspondente à coluna que corresponde à coluna do jogador  

getcol :: Player ->Int
getcol (Pacman(PacState(pid,(l,col),v,ori,points,lives) a b c )) = col


-- | função que aciona todas as ações que podem ser relizadas com fantasmas

ghost::Play ->Coords->State-> State
ghost move@(Move pid o ) cords (State m listajog n) | getPacmanMode (findPlayer listajog pid) == Mega = (State newm ( pontospac2 move (alteraGhosts cords m b )) n) 
                                                    | getPacmanMode (findPlayer listajog pid) /= Mega = (State m (perdevida move listajog) n)
                                                    |otherwise = (State m listajog n)
                              where newm = pacToEmpty (findPlayer b pid) move pacEmpty
                                    b = replaceNElem (indicePlayer listajog pid 0) (novacoordenadapacman (findPlayer listajog pid) move m) listajog
                                    pacEmpty = posPacEmpty ((getPlayerCoords((findPlayer listajog pid)))) m


-- | função que altera as coordenadas do fantasma para a casa caso este tenha sido "comido" pos um pacman 

alteraGhosts:: Coords->Maze-> [Player]->[Player]
alteraGhosts _ _  [] = []
alteraGhosts cords1 m ((Pacman(PacState(pid,cords,v,ori,points,lives) a b c )):cauda) = (Pacman(PacState(pid,cords,v,ori,points,lives) a b c )) : alteraGhosts cords1 m cauda
alteraGhosts cords1 m ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda)  | (cords1 == cords) =  ((Ghost (GhoState (pid,(ghostToHouse m),v,ori,points,lives) t)):alteraGhosts cords1 m cauda)
                                                                                    |otherwise= ((Ghost (GhoState (pid,cords,v,ori,points,lives) t)) : alteraGhosts cords1 m cauda)


-- | função que faz com que caso o pacman tenha as mesmas coordenadas de um fantasma  e esteja no estado normal perca uma vida ou passe para Dying 

perdevida:: Play -> [Player] ->[Player]
perdevida _ [] = []
perdevida move@(Move i o) ((Pacman(PacState(pid,cords,v,ori,points,lives) a b c )):cauda) |(i ==pid ) && (lives == 0 ) = (Pacman(PacState(pid,cords,v,ori,points ,0) a b Dying )) :cauda
                                                                                          |(i == pid) && (lives > 0) = (Pacman(PacState(pid,cords,v,ori,points ,(lives - 1)) a b c)):cauda
                                                                                          |otherwise = (Pacman(PacState(pid,cords,v,ori,points,lives) a b c )): perdevida move cauda                                                                                         
perdevida move@(Move i o) ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda)  =(Ghost (GhoState (pid,cords,v,ori,points,lives) t )) :perdevida  move cauda


-- | função que dá os pontos ao pac caso coma um ghost e esteja no estado mega

pontospac2:: Play -> [Player] ->[Player]
pontospac2 _ [] = []
pontospac2 move@(Move i o) ((Pacman(PacState(pid,cords,v,ori,points,lives) a b c )):cauda) |i ==pid =  (Pacman(PacState(pid,cords,v,ori,points + 10 ,lives) a b c )) :cauda
                                                                                           |otherwise = (Pacman(PacState(pid,cords,v,ori,points,lives) a b c )): pontospac2 move cauda
pontospac2 move@(Move i o) ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda)  = (Ghost (GhoState (pid,cords,v,ori,points,lives) t )) :pontospac2 move cauda


-- | função que altera o estado dos fantasmas para dead e a velocidade para metade 

alterastateGhost:: [Player]->[Player]
alterastateGhost [] = []
alterastateGhost ((Pacman(PacState(pid,cords,v,ori,points,lives) a b c )):cauda) = (Pacman(PacState(pid,cords,v,ori,points,lives) a b c )) : alterastateGhost cauda
alterastateGhost ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda)  | (t == Alive) =  ((Ghost (GhoState (pid,cords,v/2,ori,points,lives) Dead )):alterastateGhost cauda)
                                                                               |otherwise=(Ghost (GhoState (pid,cords,v,ori,points,lives) t) : alterastateGhost cauda)


-- | função que verifica se ha algum fantasma na lista de jogadores com as mesmas coordenadas do pacman que ja são dadas  

checkCoords :: Coords -> [Player] -> Bool 
checkCoords co [] = False
checkCoords co ((Pacman(PacState(pid,cords,v,ori,points,lives) a b c )):cauda) = checkCoords co cauda
checkCoords co ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda) | co==cords = True
                                                                            | otherwise = checkCoords co cauda 


-- | dado um jogador dá-nos o ghostmode ou seja se está Dead ou Alive 

getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState a b )) = b 


-- | função que dado um labirinto nos dá as coordenadas dentro da casa dos fantasmas 

ghostToHouse :: Maze -> Coords 
ghostToHouse (m:ms) |even (length (m:ms)) = ((div (length (m:ms)) 2),(div(length m)2))
                    |otherwise =(((div (length (m:ms)) 2)+1),(div(length m)2))


-- | função que dadas umas coordenadas e uma jogada nos dá as novas coordenadas

novaCord :: Coords -> Play ->Maze-> Coords
novaCord (l,col) (Move i o) m | (col == 0) && (o == L) = (l,((length(head m))-1))
                              | (col == (length (head m)-1)) && (o == R) = (l,0) 
                              | (o==U) = ((l-1),col)
                              | (o==D) = ((l+1),col)
                              | (o==R) = (l,(col+1))
                              | otherwise = (l,(col-1))                                                           
  

-- | função que nos dá o player com os pontos atualizados,e coordenadas onde a orientação da play e do pacman já foi testada anteriormente 

novacoordenadapacman :: Player -> Play -> Maze -> Player
novacoordenadapacman pac@(Pacman (PacState (pid,(l ,col),v,ori,points,lives) a b c)) (Move  i o ) m
                                                                              | col== 0  &&  o == L  = cordtunell pac m
                                                                              | col== (length (head m)-1) && o == R = cordtunelr pac 
                                                                              | o==U && ((encontra ((l-1),col) m) == Food Big) = (Pacman (PacState (pid,((l-1),col),v*2,U,points+5,lives) (a+(7.0)) b Mega))
                                                                              | o==D && ((encontra ((l+1),col) m ) == Food Big) = (Pacman (PacState (pid,((l+1),col),v*2,D,points+5,lives) (a+(7.0)) b Mega))
                                                                              | o==L &&((encontra (l ,(col-1))m ) == Food Big) = (Pacman (PacState (pid,(l,(col-1)),v*2,L ,points+5,lives) (a+(7.0)) b Mega))
                                                                              | o==R && ((encontra (l,(col+1)) m ) == Food Big) = (Pacman (PacState (pid,(l,(col+1)),v*2,R,points+5,lives) (a+(7.0)) b Mega))
                                                                              | o==R && ((encontra (l,(col+1)) m ) == Food Little) = (Pacman (PacState (pid,(l,(col+1)),v,R,points+1,lives) a b c))
                                                                              | o==L &&((encontra (l ,(col-1)) m ) == Food Little) = (Pacman (PacState (pid,(l,(col-1)),v,L ,points+1,lives) a b c))
                                                                              | o==D && ((encontra ((l+1),col) m ) == Food Little) = (Pacman (PacState (pid,((l+1),col),v,D,points+1,lives) a b c))
                                                                              | o==U && ((encontra ((l-1),col) m) == Food Little) = (Pacman (PacState (pid,((l-1),col),v,U,points+1,lives) a b c))
                                                                              | o==R && ((encontra (l,(col+1)) m ) == Empty) = (Pacman (PacState (pid,(l,(col+1)),v,R,points,lives) a b c))
                                                                              | o==L && ((encontra (l ,(col-1)) m ) == Empty) = (Pacman (PacState (pid,(l,(col-1)),v,L ,points,lives) a b c))
                                                                              | o==D && ((encontra ((l+1),col) m ) == Empty) = (Pacman (PacState (pid,((l+1),col),v,D,points,lives) a b c))
                                                                              | o==U && ((encontra ((l-1),col) m) == Empty) = (Pacman (PacState (pid,((l-1),col),v,U,points,lives) a b c))
                                                                              | otherwise = (Pacman (PacState (pid,(l,col),v,ori,points,lives) a b c))
novacoordenadapacman gho@(Ghost (GhoState (pid,(l,col),v,ori,points,lives) t )) (Move  i o ) m 
                                                                              | o==U  = (Ghost (GhoState (pid,((l-1),col),v,ori,points,lives) t ))
                                                                              | o==D  = (Ghost (GhoState (pid,((l+1),col),v,ori,points,lives) t ))
                                                                              | o==L  = (Ghost (GhoState (pid,(l,(col-1)),v,ori,points,lives) t ))
                                                                              | o==R  = (Ghost (GhoState (pid,(l,(col+1)),v,ori,points,lives) t ))
                                                                              |otherwise= (Ghost (GhoState (pid,(l,col),v,ori,points,lives) t ))


  
-- | função que encontra a peça correspondente a coordenadas dadas no labirinto 

encontra :: (Int,Int)-> Maze-> Piece
encontra (l,col) (h:hs) | (l == 0 ) = (!!) h col
                        | otherwise = encontra (l-1,col) hs  


-- | função que testa se é possivel movimentar dadas uma Play e umas coordenadas 

testaParede :: Play -> (Int,Int) -> Maze ->Bool
testaParede (Move i o) (l,col) m | o==L && (col == 0 ) = True
                                 | o==R && (col == length (head m)-1) = True  
                                 | o==R && ((encontra (l,(col+1)) m ) == Wall) = False
                                 | o==L && ((encontra (l ,(col-1))m ) == Wall) = False
                                 | o==D && ((encontra ((l+1),col) m ) == Wall) = False
                                 | o==U && ((encontra ((l-1),col) m) == Wall) = False
                                 | otherwise = True


-- | função que troca a orientaçaõ de um jogador caso esta seja diferente da orientação da jogada

coordenadaEtrocaOrientacao :: Player -> Play -> Player 
coordenadaEtrocaOrientacao (Pacman (PacState (pid,(l,col),v,ori,points,lives) a b c)) (Move  i o ) 
                                                                              | (o == U) && (ori/=U) = (Pacman (PacState (pid,(l,col),v,o,points,lives) a b c))
                                                                              | (o == D) && (ori/=D) = (Pacman (PacState (pid,(l,col),v,o,points,lives) a b c))
                                                                              | (o == L) && (ori/=L) = (Pacman (PacState (pid,(l,col),v,o,points,lives) a b c))
                                                                              | (o == R) && (ori/=R) = (Pacman (PacState (pid,(l,col),v,o,points,lives) a b c))
                                                                              | otherwise = (Pacman (PacState (pid,(l,col),v,ori,points,lives) a b c)) 


 -- | função que substitui a casa onde o pacman estava por uma casa vazia  

pacToEmpty :: Player-> Play -> Maze -> Maze
pacToEmpty p pl m | (testaParede pl (getPlayerCoords p) m ==True) =  replaceElemInMaze (getPlayerCoords p) Empty m
                  | otherwise = m


-- | função que altera o maze e atualiza a posição anterior do fantasma no mapa

posPacEmpty :: Coords -> Maze -> Maze 
posPacEmpty c m = replaceElemInMaze c Empty m


-- | função que altera as coordenadas de um player caso esteja em posição de passar o tunel da direita

cordtunelr::Player ->Player
cordtunelr (Pacman(PacState(pid,(l,col),v,ori,points,lives) a b c)) = (Pacman(PacState(pid,(l,0),v,ori,points,lives) a b c)) 
cordtunelr (Ghost (GhoState (pid,(l,col),v,ori,points,lives) t )) = (Ghost (GhoState (pid,(l,0),v,ori,points,lives) t ))


-- | função que altera as coordenadas de um player caso esteja em posição de passar o tunel da esquerda

cordtunell::Player->Maze->Player
cordtunell (Pacman(PacState(pid,(l,col),v,ori,points,lives) a b c)) m = (Pacman(PacState(pid,(l,((length(head m))-1)),v,ori,points,lives) a b c))
cordtunell (Ghost (GhoState (pid,(l,col),v,ori,points,lives) t )) m = (Ghost (GhoState (pid,(l,((length(head m))-1)),v,ori,points,lives) t ))



js = [jogador1,jogador3,jogador2]
jogador1=(Pacman (PacState (0,(5,1),1,R,4,1) 0 Open Normal))
jogador3=(Ghost  (GhoState (2,(7,1),1,L,0,1) Alive ))
jogador2=(Ghost  (GhoState (5,(5,10),1,R,0,1) Alive ))


maze3 = [

    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
    [Empty, Food Big, Wall, Empty,Empty, Empty, Empty, Wall, Food Little, Wall,Wall, Wall, Empty, Wall, Wall, Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Empty, Food Big, Wall, Empty,Empty, Empty, Empty, Wall, Food Little, Wall,Empty, Empty, Empty, Empty, Empty, Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
    [Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Food Little, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Food Little,Wall, Wall, Wall, Wall, Wall, Wall,Food Little,Wall],
    [Wall, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little ,Food Little, Food Little,Food Little, Food Little, Food Little, Food Little, Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Little,Food Big,Wall],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall, Wall, Wall, Wall]]
