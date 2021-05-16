module TypesPacman where

-- Tipos da tarefa 1

type Maze = [Corridor] -- sempre horizontal
type Corridor = [Piece]

-- Tipos da tarefa 1 e da tarefa 2

data Piece     = Food FoodType | PacPlayer Player| Empty | Wall  deriving (Eq)
data FoodType  = Big | Little deriving (Show,Eq)

instance Show Piece where
     show (Food Big) = "o"
     show (Food Little) = "."
     show (Wall) = "#"
     show (Empty) = " "


data Player    = Pacman PacState | Ghost GhoState deriving (Eq, Show)

-- Tipos exclusivos da tarefa 2

data Play = Move Int Orientation deriving (Show,Eq)
data Orientation = L | R | U | D deriving (Show,Eq)

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double, Orientation, Int, Int)
data Mouth = Open | Closed deriving (Show,Eq)
data PacMode = Dying | Mega | Normal deriving (Show,Eq)
data GhostMode = Dead | Alive deriving (Show,Eq)

data State = State 

    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    
    } deriving (Eq)

data PacState = PacState

    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving (Show,Eq)

data GhoState = GhoState 

    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    
    } deriving (Show,Eq)

-- Tipos da tarefa 3

type Instructions = [Instruction]
data Instruction = Instruct [(Int, Piece)]
               | Repeat Int