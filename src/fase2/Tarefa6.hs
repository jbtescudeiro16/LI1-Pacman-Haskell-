
{- |

==Introdução / Objetivos 

 * O grande objetivo da tarefa 6 era implementar um robot (bot) que jogasse pacman da maneira mais eficaz possível e similar a um jogador humano. 
Assim a tarefa foi feita de modo a implementar funções para os 2 modos do pacman jogáveis , (Normal e Mega ). 

 

==Estratégia 

1.Primeiro foram feitas as funções necessárias para que o pacman no modo normal, procurasse incrementar os seus pontos através da ingestão de foods (Big ou little). 

2.Em segundo lugar, e em complemento do primeiro ponto, calculou-se a distância “segura” para o pacman puder procurar “foods”. A distância foi definida pelo grupo como 3 unidades, e caso um ghost se aproximasse demasiado o pacman entrava em modo fuga. 

3.Por último definiram-se as funções necessárias para que o pacman, em modo Mega, perseguisse os fantasmas de modo a incrementar os seus pontos e enviá-los de volta á casa de partida.

 

==Discussão e conclusão 

Na opinião do grupo esta foi a tarefa mais divertida de todas, pois levou-nos a pensar as estratégias utilizadas por cada um quando joga o jogo, e implementá-las no nosso código de modo a que o nosso robot se tornasse um bot que conseguisse fazer boas pontuações. 

Concluindo o bot podia ter inúmeras estratégias diferentes, pensadas por outros jogadores, mas o nosso executa apenas as estratégias pensadas por cada elemento do grupo.


-}


module Tarefa6 where 

import Types
import Tarefa2
import Tarefa5



-- | Executa as varias plays ao jogador do tipo pacman dependo dos estados 

botMovesPac :: Int -> State -> Maybe Play
botMovesPac id (State m ps l) | (id==getPlayerID pac) && ((getPacmanMode pac) == Dying) = Nothing
                              | (id==getPlayerID pac) && ((getPacmanMode pac) == Normal) = Just (findPoints id (State m ps l))
                              | (id==getPlayerID pac) && ((getPacmanMode pac) == Mega) = Just (trytoChase id (State m ps l))
                              | otherwise = Nothing 
                        where pac = findpac ps



-- | Procura comida (little or big) no mapa de modo a incrementar os seus pontos e passar o nivel se a dist>3 ou foge do ghost

findPoints :: Int -> State -> Play
findPoints id (State m ps l) | (distPacGhost pac zeca) > 3.0 = playcerta pac m
                             | otherwise = runNow pac zeca m          
                    where pac = findpac ps
                          zeca = findGhostclose pac ps




-- | Calcula a distancia entre o pacman e o ghost mais proximo

distPacGhost :: Player -> Player -> Float
distPacGhost pac zeca = sqrt ( fromIntegral((d-b)^2) + fromIntegral((c-a)^2) )
               where (a,b) = getPlayerCoords pac
                     (c,d) = getPlayerCoords zeca 




-- | Funçao que direciona o pacman de modo a fugir do ghost mais proximo

runNow :: Player -> Player -> Maze -> Play 
runNow pac zeca m | ((encontra (lp,colp+1) m) /= Wall) && (lp == lg) && ((colp - colg) >0) = (Move id R)
                  | ((encontra (lp,colp-1) m) /= Wall) && (lp == lg) && ((colp - colg) <0) = (Move id L)
                  | ((encontra (lp+1,colp) m) /= Wall) && (colp == colg) && ((lp-lg)>0) = (Move id D)
                  | ((encontra (lp-1,colp) m) /= Wall) && (colp == colg) && ((lp-lg)<0) = (Move id U)
                  | ((encontra (lp-1,colp) m) /= Wall) && (abs (lp-lg) < abs (colp-colg) ) && (lp-lg < 0) = (Move id U)
                  | ((encontra (lp+1,colp) m) /= Wall) && (abs (lp-lg) < abs (colp-colg) ) && (lp-lg > 0) = (Move id D)
                  | ((encontra (lp,colp-1) m) /= Wall) && (abs (lp-lg) > abs (colp-colg) ) && (colp-colg < 0) = (Move id L)
                  | ((encontra (lp,colp+1) m) /= Wall) && (abs (lp-lg) > abs (colp-colg) ) && (colp-colg < 0) = (Move id R)
                  | otherwise = (Move id R)
                  where (lp,colp) = getPlayerCoords pac
                        (lg,colg) = getPlayerCoords zeca
                        id = getPlayerID pac



-- | Desloca-se para a food mais proxima conforme a orientaçao do pacman ( funciona apenas se a comida estiver nas 3 casas mais proximas , U D R L)

playcerta :: Player -> Maze -> Play
playcerta pl m | ((testaFoodR (l,col+1) m) == True) = (Move id R)
               | ((testaFoodL (l,col-1) m) == True) = (Move id L)
               | ((testaFoodU (l-1,col) m) == True) = (Move id U)
               | ((testaFoodD (l+1,col) m) == True) = (Move id D)
               | otherwise = procuraNextFood pl 1 m
            where id = getPlayerID pl
                  (l,col) = getPlayerCoords pl
                  ori = getPlayerOrientation pl



-- | C = contador (começa em 1) e procura a comida mais proxima

procuraNextFood :: Player -> Int -> Maze -> Play
procuraNextFood pl c m | ((encontra (l ,(col-c))m ) == Food Little) = (Move pid L)
                       | ((encontra (l ,(col-c))m ) == Food Big) = (Move pid L)
                       | ((encontra (l ,(col+c))m ) == Food Little) = (Move pid R)
                       | ((encontra (l ,(col+c))m ) == Food Big) = (Move pid R)
                       | ((encontra ((l-c),col) m) == Food Little) = (Move pid U)
                       | ((encontra ((l-c),col) m) == Food Big) = (Move pid U)
                       | ((encontra ((l+c),col) m) == Food Little) = (Move pid D)
                       | ((encontra ((l+c),col) m) == Food Big) = (Move pid D)
                       | otherwise = procuraNextFood pl (c+1) m
                where pid = getPlayerID pl
                      (l,col) = getPlayerCoords pl



-- | Testa à  esquerda da posiçao do pacman se encontra comida (big or little)

testaFoodL :: (Int,Int) -> Maze -> Bool
testaFoodL (l,col) m | ((encontra (l ,(col-1))m ) == Food Little) = True
                     | ((encontra (l ,(col-1))m ) == Food Big) = True
                     | otherwise = False



-- | Testa à direita da posiçao do pacman se encontra comida (big or little)

testaFoodR :: (Int,Int) -> Maze -> Bool
testaFoodR (l,col) m | ((encontra (l,(col+1)) m ) == Food Little) = True
                     | ((encontra (l,(col+1)) m ) == Food Big) = True
                     | otherwise = False



-- | Testa se em cima da posiçao do pacman se encontra comida (big or little)

testaFoodU :: (Int,Int) -> Maze -> Bool
testaFoodU (l,col) m | ((encontra ((l-1),col) m) == Food Little) = True
                     | ((encontra ((l-1),col) m) == Food Big) = True
                     | otherwise = False



-- | Testa se em baixo da posiçao do pacman se encontra comida (big or little)

testaFoodD :: (Int,Int) -> Maze -> Bool
testaFoodD (l,col) m | ((encontra ((l+1),col) m ) == Food Little) = True
                     | ((encontra ((l+1),col) m ) == Food Big) = True
                     | otherwise = False



-- | Persegue os fantasmas no mapa de modo a incrementar os seus pontos

trytoChase :: Int -> State -> Play
trytoChase id (State m ps l) = alterajogadas pac zeca
               where pac = findpac ps
                     zeca = findGhostclose pac ps 



-- | Devolve o ghost que se encontra mais proximo do pacman no mapa

findGhostclose :: Player -> [Player] -> Player
findGhostclose pac lista = (closer pac cordG) 
              where pac = getPlayerCoords (findpac lista)
                    cordG = getGhosts lista



-- | Recebe uma lista de players e devolve uma lista de jogadores do tipo Ghost

getGhosts :: [Player] -> [Player]
getGhosts [] = []
getGhosts (ghoste@(Ghost (GhoState (pid,cords,v,ori,points,lives) a)):ls) = ghoste : getGhosts ls
getGhosts ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a b c)):ls) = getGhosts ls



-- | Compara entre os fantasmas qual deles esta mais proximo do pacman 

closer :: Coords -> [Player] -> Player
closer (l,col) [x] = x
closer (l,col) (k:j:ts) | (((a-l)^2) + ((b-col)^2)) < (((c-l)^2) + ((d-col)^2)) = closer (l,col) (k:ts)
                        | otherwise = closer (l,col) (j:ts)
            where (a,b) = getPlayerCoords k
                  (c,d) = getPlayerCoords j



-- | Direciona o pacman em direçao ao ghost

alterajogadas ::Player ->Player -> Play 
alterajogadas pac zeca | (lp == lg) && ((colp - colg) >0) = (Move id L)
                       | (lp == lg) && ((colp - colg) <0) = (Move id R)
                       | (colp == colg) && ((lp-lg)>0) = (Move id U)
                       | (colp == colg) && ((lp-lg)<0) = (Move id D)
                       | (abs (lp-lg) < abs (colp-colg) ) && (lp-lg < 0) = (Move id D)
                       | (abs (lp-lg) < abs (colp-colg) ) && (lp-lg > 0) = (Move id U)
                       | (abs (lp-lg) > abs (colp-colg) ) && (colp-colg < 0) = (Move id R)
                       | otherwise = (Move id L)
             where (lp,colp) = getPlayerCoords pac
                   (lg,colg) = getPlayerCoords zeca
                   id = getPlayerID pac



-- | Funçao final

bot :: Int -> State -> Maybe Play
bot id s = botMovesPac id s
