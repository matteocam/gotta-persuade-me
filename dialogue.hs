module Dialogue where

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


-- XXX: Hey! shouldn't agents have access to the dialogue too??
class Player a where 
  makeDialogueMove :: a -> Move

data PlayerRole = P | O deriving (Show, Eq)

otherPlayer P = O
otherPlayer O = P

-- Moves
data Move  = Move {ident :: Id, 
                  s :: CL, -- speech act
                  pl :: PlayerRole,
                  t :: Id -- target
                  } deriving (Show, Eq)
            
type Moves = [Move]
lastMove :: Moves -> Move
lastMove [] = error "Empty Move set"
lastMove x = head x

containsMove :: Move -> Moves -> Bool
containsMove = elem

emptyMoves :: Moves -> Bool  
emptyMoves = null

insertMove :: Move -> Moves -> Moves
insertMove = (:)

-- Dialogue Systems
data Player a => Dialogue a = Dialogue {topic :: LL,
                            moves :: Moves,
                            proponent :: a,
                            opponent :: a}


mkEmptyDialogue ::Player a => LL -> a -> a  -> Dialogue a
mkEmptyDialogue t p o = Dialogue t [] p o


type TurnTakingFunction a = Dialogue a -> PlayerRole


-- alternateTurns
simpleTurn :: Player a => TurnTakingFunction a
simpleTurn d = if emptyMoves (moves d) then
			          P
				       else
		            otherPlayer (pl $ lastMove $ moves d)

-- function used for turnTaking
turnTaker d = case simpleTurn d of
                P -> proponent d
                O -> opponent d


type Outcome a = Dialogue a

-- Outcome of a dialogue
-- XXX: for now it is just the dialogue itself
outcome :: Dialogue a -> Outcome a
outcome = id

terminated :: Player a => Dialogue a -> Bool
terminated d = length (moves d) > 3 -- XXX

-- a dialogue system takes into account things like turns or legal moves
-- data DialogueSystem = DS {isLegalMove :: Move -> Bool, turn} 
-- legalMoves :: DialogueSystem -> Moves -> Moves = 
-- legalMoves ds = filter (isLegalMove ds)

dialogueProtocol' :: Player a => Dialogue a -> Outcome a
-- XXX: topic of the dialogue
dialogueProtocol' d = if terminated d then
		                    outcome d
	      		          else 
                        dialogueProtocol' $ dialogueStep d (turnTaker d)

                     
dialogueProtocol :: (Player a) =>  LL -> a -> a -> Outcome a
dialogueProtocol  t p o = dialogueProtocol' $ mkEmptyDialogue t p o
               

-- given a dialogue and a player that is supposed to move it returns another dialogue
-- XXX: where should the check for legalMoves go?
-- XXX: how much knowledge does the player have access to?
dialogueStep :: Player a =>  Dialogue a -> a -> Dialogue  a
dialogueStep dialogue turnPlayer = let newMove = makeDialogueMove turnPlayer
                                       moves' = insertMove newMove (moves dialogue)
                                   in dialogue { moves = moves'}



