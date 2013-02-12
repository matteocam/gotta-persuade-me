
-- Languages
type LogicalLanguage = String
type LL = LogicalLanguage 
data CommunicationLanguage = Claim LL
                             | Why LL 
                             | Since LL [LL] 
                             | Concede LL 
                             | Retract LL deriving (Show, Eq)
                                                   
type CL = CommunicationLanguage
type Id = Int

data Player = P | O


-- Moves
data Move = Move {id :: Id, 
                  s :: CL, -- speech act
                  pl :: Player,
                  t :: Id -- target
                  } deriving (Show, Eq)
            
type Moves = [Move]
lastMove :: Moves -> Move
lastMove [] = error "Empty Move set"
lastMove = head

containsMove :: Move -> Moves -> Bool
containsMove = elem

emptyMoves :: Moves -> Bool  
emptyMoves = null

insertMove :: Move -> Moves -> Moves
insertMove = : 

-- Dialogue Systems
data Dialogue = Dialogue {topic :: LL, moves :: Moves, turn :: Dialogue -> Player }

type TurnTakingFunction = Dialogue -> Player

-- alternateTurns
simpleTurn :: TurnTakingFunction
simpleTurn d = otherPlayer (pl $ lastMove $ moves d)

-- a dialogue system takes into account things like turns or legal moves
-- data DialogueSystem = DS {isLegalMove :: Move -> Bool, turn} 
-- legalMoves :: DialogueSystem -> Moves -> Moves = 
-- legalMoves ds = filter (isLegalMove ds)
dialogue :: Player -> Player -> Outcome 
-- XXX: topic of the dialogue?
dialogue p o = let dialogue' = undefined
               in dialogue' emptyDialogue turnPlayer = turn dialogue

-- given a dialogue and a player that is supposed to move it returns another dialogue
-- XXX: where should the check for legalMoves go?
-- XXX: how much knowledge does the player have access to?
dialogueStep :: Dialogue -> Player -> Dialogue 
dialogueStep dialogue turnPlayer = let newMove = nextMove turnPlayer
                                       moves' = insertMove newMove moves
                                   in dialogue { moves = moves'}



