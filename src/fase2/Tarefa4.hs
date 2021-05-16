
{-| 

==Introdução

*Esta tarefa consistiu na implementação da passagem do tempo no pacman.Esta tarefa foi a mais díficil para o grupo pois apesar de muitas tentativas ,
não foi conseguido o totalmente objetivo final ,sendo esta a única tarefa inacabada.


==Objetivos

*O grande objetivo desta tarefa foi tentar implementar as velocidades de forma correta a par da passagem do tempo . 
*Apesar das diversas tentativas , o grupo não conseguiu encontrar o erro que impedia o funcionamento na perfeição.


==Discussão e Conclusão   

*A grande limitação desta tarefa foi maioritariamente a aplicação recursiva de funções onde dificilmente ocorre recursividade , pois apesar de todas
as funções funcionarem na perfeição para o que lhes é pedido , foi impossível concluir na perfeição esta tarefa .

Em suma,apesar da tarefa não estar concluída e da perspetiva do grupo ,apresenta todas as funções necessárias para que a tarefa ficasse totalmente concluída.

 -}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5



-- | Tempo padrão

defaultDelayTime = 250 



-- | Vai alterando o estado com o passar do tempo 

passTime :: Int  -> State -> State
passTime x s@( State m listajog l) = analisator (aplicajogada (ghostPlay l) l)
                                 where l = recAnalisavel x listajog s



-- | Aplica as jogadas dos fantasmas 

aplicajogada :: [Play]->State ->State 
aplicajogada [] x = x 
aplicajogada (h:t) (State m ps l) = aplicajogada t  (play h (State m ps l ))



-- | Função que repete  as jogadas dependendo da velocidade , nas respetivas iterações

playagain :: Int ->State ->Player ->State
playagain 0 s p = s 
playagain n s p = (playagain (n-1) (play (Move id o) s ) p)
      where id = getPlayerID p
            o = getPlayerOrientation p 


-- | Função que analisa a velocidade e os steps,e aplica jogadas 

analisavel ::Int-> Player->State ->State
analisavel st p s | (v == 1.0) = play (Move pid ori) s  
                  | (v == 0.5) && (mod st 2 == 0) = play (Move pid ori) s 
                  | (v == 2.0) = playagain 2 s p 
                  | (v == 1.5) && (mod st 2 == 0) = playagain 3 s p 
                  | otherwise = play (Move pid ori) s  
        where v = sacavelo p
              pid = getPlayerID p
              ori = getPlayerOrientation p 
   


-- | Aplica recursividade para todos os jogadores através da analisavel

recAnalisavel:: Int ->[Player]->State -> State 
recAnalisavel _ [] s = s 
recAnalisavel st (h:t) s  = recAnalisavel st t (analisavel st h s)



-- | Função que une as auxiliares que alteram a boca e o estado dos jogadores 

analisator ::State->State
analisator (State m ps l) = (State m (aux20 (alterafanta (aux1 ps) b )) l) 
       where b = getPacmanMode (findpac (aux1 ps) )



-- | Se o pacman estiver em modo Mega, altera o estado dos fantasmas  

alterafanta :: [Player] -> PacMode -> [Player]
alterafanta l Mega = l
alterafanta l Normal = alteraalive l 



-- | Altera a abertura da boca do pacman  

aux20:: [Player] -> [Player]
aux20 ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a Closed c)):cauda) = ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a Open c)) :cauda)
aux20 ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a Open c)):cauda) = ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a Closed c)):cauda)
aux20 ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )):cauda) = ((Ghost (GhoState (pid,cords,v,ori,points,lives) t )) : aux20 cauda)



-- | Altera o estado,velocidade ou o tempo Mega do pacman    

aux1 :: [Player]->[Player]
aux1 [] = []
aux1 ((Pacman (PacState (pid,(l,col),v,ori,points,lives) a b c)):t) = if (a> 0) 
                                                                      then ((Pacman (PacState (pid,(l,col),v,ori,points,lives) (a- 0.250) b c)) :t)  
                                                                      else ((Pacman (PacState (pid,(l,col),v/2,ori,points,lives) 0 b Normal)):t)
aux1 (a:t) = a:(aux1 t) 



-- | Quando o pacman passa para modo Normal , os fantasmas passam de modo Dead para modo Alive e a sua velocidade passa para o dobro 

alteraalive:: [Player]->[Player]
alteraalive [] = []
alteraalive ((Ghost (GhoState (pid,cords,v,ori,points,lives) Dead )):cauda) =  ((Ghost (GhoState (pid,cords,(v*2),ori,points,lives) Alive)):alteraalive cauda) 
alteraalive (a:cauda) = a : alteraalive cauda



-- | Função que dado um jogador devolve a sua velocidade

sacavelo :: Player ->Double 
sacavelo (Pacman(PacState(pid,cords,v,ori,points,lives) a b c )) = v
sacavelo (Ghost (GhoState (pid,cords,v,ori,points,lives) t )) = v

