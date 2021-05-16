module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k (Manager (State m listajog l ) p st b dlt dly) 
                                                          | k == KeyLeftArrow  = trocaOrientacao (Manager (State m listajog l ) p st b dlt dly)  (Move  p L )
                                                          | k == KeyRightArrow = trocaOrientacao (Manager (State m listajog l ) p st b dlt dly)  (Move  p R )
                                                          | k == KeyUpArrow    = trocaOrientacao (Manager (State m listajog l ) p st b dlt dly)  (Move  p U )
                                                          | k == KeyDownArrow  = trocaOrientacao (Manager (State m listajog l ) p st b dlt dly)  (Move  p D )

trocaOrientacao :: Manager -> Play -> Manager 
trocaOrientacao  (Manager (State m listajog l ) pid st b dlt dly)  (Move  p o ) = (Manager (State m novalista l) p st b dlt dly )
                                     where novalista = (replaceNElem (indicePlayer listajog p 0) (coordenadaEtrocaOrientacao (findPlayer listajog p) (Move p o)) listajog)


updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
        
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- | Before passa a now e o delta passa à soma do delta com o tempo atual menos o que estavamos 

updateTime :: Integer -> Manager -> Manager
updateTime now ( Manager s p st b dlt dly) = Manager s p st now (dlt+(now-b)) dly

-- | Função que dà reset no time,ou seja o delta passa a 0 

resetTimer :: Integer -> Manager -> Manager
resetTimer now ( Manager s p st b dlt dly) = (Manager s p st now 0 dly)

-- | Função que aplica a pssagem do tempo verificando a proxima "frame"
nextFrame :: Integer -> Manager -> Manager
nextFrame now ( Manager s p st b dlt dly) =  resetTimer now ( Manager (passTime st s ) p st b dlt dly) 


loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

