module Dialogue.Protocol where

import Dialogue.Languages
import Dialogue.Move
import Dialogue.Commitments


-- XXX: this might go somewhere else
class Player a where 
  makeDialogueMove :: Dialogue a -> a -> Move


-- Dialogue Systems
data Dialogue a = Dialogue {topic :: LL,
                            moves :: Moves,
                            proponent :: a,
                            opponent :: a,
                            cmstores :: CSs} deriving Show
 


emptyDialogue :: Dialogue a
emptyDialogue  = Dialogue { topic = undefined, moves = startingMoves, 
                            proponent = undefined, opponent = undefined,
                            cmstores = emptyCSs }


-- Turntaking functions
type TurnTakingFunction a = Dialogue a -> PlayerRole


-- alternateTurns
simpleTurn :: TurnTakingFunction a
simpleTurn d = if emptyMoves (moves d) then
                 P
               else
                 otherPlayer (pl $ lastMove $ moves d)
                 
-- used for knowing about the current turntaker's role.
-- For the actual player, you should use turnTaker
globalTurnFunction = simpleTurn -- XXX: change if needed

-- function used for turnTaking
turnTaker :: Dialogue a ->  a
turnTaker d = case globalTurnFunction d of
                P -> proponent d
                O -> opponent d

-- Termination
 
type Outcome a = Dialogue a

-- Outcome of a dialogue
-- XXX: for now it is just the dialogue itself
outcome :: Dialogue a -> Outcome a
outcome = id

terminated :: Player a => Dialogue a -> Bool
terminated d = length (moves d) > 10 -- XXX

-- Actual Protocol

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

                     
-- TODO: Have you ever thought of this with something with a Writer/Log Monad?

dialogueProtocol :: (Player a) =>  LL -> a -> a -> Outcome a
dialogueProtocol  t p o = dialogueProtocol' $ 
                          emptyDialogue {
                            topic = t,
                            proponent = p,
                            opponent = o }
               

-- given a dialogue and a player that is supposed to move it returns another dialogue with the new move
-- XXX: where should the check for legalMoves go?
-- XXX: how much knowledge does the player have access to?
-- XXX: this function should be factored and its responsibilities split
dialogueStep :: Player a =>  Dialogue a -> a -> Dialogue  a
dialogueStep dialogue turnPlayer = let newMove = makeDialogueMove dialogue turnPlayer -- a player moves
                                       moves' = insertMove newMove (moves dialogue) -- the move is added to the previous ones
                                       -- XXX: the next is ugly: you're using the turn function which is not supposed to be here)
                                       cmstores' = updatePlayerCS (globalTurnFunction dialogue) newMove (cmstores dialogue)
                                   in dialogue { moves = moves'} {cmstores = cmstores'} -- we return a new updated dialogue 



-- returns the last speech act in d from the player with role r
lastUtterance :: Dialogue a -> PlayerRole -> Move
lastUtterance d r = case movesFromPlayer of
                         [] -> DummyMove 
                         _ -> lastMove movesFromPlayer
 where isSamePlayer = (== r) . pl 
       movesFromPlayer = filter isSamePlayer (init $ moves d)
