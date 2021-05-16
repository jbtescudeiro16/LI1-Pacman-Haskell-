
{-|
== Introdução/Objetivos
A tarefa 5 consistia em implementar as movimentações nos fantasmas , mediante uma escolha da melhor estratégia pelo grupo 

==Estratégias
Assim e mediante o pedido o grupo decidiu implementar as seguintes estratégias.

*Caso o fantasma se encontre na casa dos fantasmas , este sai da mesma.

*Caso o pacman se encontre em modo mega os fantasmas usam uma estratégia que consiste em manter o movimento até encontrarem uma parede . 

*Quando encontra a mesma este obtem o movimento nos sentidos do ponteiro do relogio :

  1.se a sua orientação for para a direita estes ficam orientados para baixo ;

  2.se a sua orientação for para baixo este fica orientado para a esquerda ;

  3.se a sua orientação for para a esquerda este fica orientado para cima ; 

  4.se a sua orientação for para cima este fica orientado para a direita ;

*Caso o pacman esteja em modo Normal , os  fantasmas entram em modo de perseguição em que mantêm o seu movimento  até encontrarem uma parede. 
Neste momento os fantasmas avaliam a posição do pacman  e escolhem qual a direção a tomar mediante a posição mais próxima do pacman .

==Discussão e Conclusão

Apesar da tarefa 5 estar conluída na perfeição , mediante a estratégia escolhida pelo grupo , estas movimentações não conseguiram ser implementadas na sua 
totalidade face ao facto da tarefa 4 não estar acabada na sua totalidade.

-}


module Tarefa5 where 

import Types
import Tarefa2


-- | Função que aplica todas as jogadas a todos os fantasmas

ghostPlay :: State -> [Play]
ghostPlay state@(State m ps l ) = auxghostPlay state a
             where a = aplicaPLaytoGhost ps



-- | Funçao que devolve uma lista de jogadores apenas com jogadores do tipo fantasma

aplicaPLaytoGhost :: [Player]-> [Player]
aplicaPLaytoGhost [] = []
aplicaPLaytoGhost (l:ls) | (aux l == True) = l : aplicaPLaytoGhost ls
                         | otherwise = aplicaPLaytoGhost ls



-- | Verifica se o player é do tipo pacman ou ghost

aux :: Player ->Bool
aux (Pacman h) = False
aux (Ghost _ ) = True 



-- | Função que acplica a todos os fantasmas a função saidatoca 

auxghostPlay :: State -> [Player] ->[Play]
auxghostPlay s [] = []
auxghostPlay state@(State m ps l) (h:t) =  saidatoca state h : auxghostPlay (State m ps l) t 



-- | Função que retira os fantasmas da casa 

saidatoca ::State -> Player -> Play 
saidatoca s@(State m ps l) p | (getPlayerCoords p) == ghostToHouse m = (Move i U) 
                             | otherwise = decidechaseouscat s p
                          where i = getPlayerID p 



-- | Função que decide ,mediante o estado do pacman, se entra no modo perseguição ou modo de fuga

decidechaseouscat :: State-> Player ->Play
decidechaseouscat (State m ps l) p | (getPacmanMode (findpac ps) == Mega) =  scatterMode (State m ps l) i 
                                   | otherwise = chaseMode (State m ps l ) i 
                where i = getPlayerID p 



-- | Auxiliar à decidechaseouscat

findpac :: [Player] ->Player
findpac ((Pacman h) :t ) = (Pacman h)
findpac ((Ghost _ ):t) = findpac t 



-- | Função que verifica as condições de perseguição do fantasma 

chaseMode :: State -> Int -> Play
chaseMode (State m ps l) i | (encontra newcord m ) /= Wall = (Move i ori)
                           | otherwise = alterajogada pac zeca
       where a =  getPlayerCoords (zeca)
             zeca = findPlayer ps i
             ori = getPlayerOrientation zeca
             pac = findpac ps 
             b = getPlayerCoords pac
             newcord = novaCord a (Move i ori) m 



-- | Função que altera a orintação do ghost quando este encontra uma parede (no modo de perseguição) 

alterajogada ::Player ->Player -> Play 
alterajogada pac zeca  | (lp == lg) && ((colp - colg) >0) = (Move id R)
                       | (lp == lg) && ((colp - colg) <0) = (Move id L)
                       | (colp == colg) && ((lp-lg)>0) = (Move id D)
                       | (colp == colg) && ((lp-lg)<0) = (Move id U)
                       | (abs (lp-lg) < abs (colp-colg) ) && (lp-lg < 0) = (Move id U)
                       | (abs (lp-lg) < abs (colp-colg) ) && (lp-lg > 0) = (Move id D)
                       | (abs (lp-lg) > abs (colp-colg) ) && (colp-colg < 0) = (Move id L)
                       | otherwise = (Move id R)
             where (lp,colp) = getPlayerCoords pac
                   (lg,colg) = getPlayerCoords zeca
                   id = getPlayerID zeca
 


-- | Função que gera jogadas para os fantasmas da lista (em caso de fuga)

scatterMode :: State -> Int -> Play
scatterMode (State m ps l) i | (encontra newcordg m) /= Wall = (Move i ori)
                             | otherwise = (alterafuga zeca)
       where a = getPlayerCoords (findPlayer ps i )
             pac = findpac ps 
             zeca = findPlayer ps i
             ori = getPlayerOrientation zeca
             newcordg = novaCord a (Move i ori) m



-- | Roda o fantasma numa direção no sentido dos ponteiros do relógio

alterafuga::Player -> Play 
alterafuga zeca |ori == U = Move id R
                |ori == D = Move id L
                |ori == R = Move id D
                |otherwise = Move id U
             where id = getPlayerID zeca 
                   ori = getPlayerOrientation zeca 



-- | Função que troca a orientação ao fantasma no momento que o ghost come uma food big

momentoficadead ::Player -> Play 
momentoficadead p |(getPlayerOrientation p) == U = (Move i D)
                  |(getPlayerOrientation p) == D = (Move i U )
                  |(getPlayerOrientation p) == L = (Move i R ) 
                  |otherwise =  (Move i L)
     where i = getPlayerID p 