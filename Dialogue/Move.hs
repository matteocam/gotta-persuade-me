module Dialogue.Move where

import Dialogue.Languages

-- Moves
data Move  = DummyMove | -- by convention a move set in a dialogue always have at leat a dumb move
             Move {
                    ident :: Id,
                    s :: CL, -- speech act
                    pl :: PlayerRole,
                    t :: Id -- target
                  } deriving (Show, Eq)
            
type Moves = [Move]
lastMove :: Moves -> Move
lastMove [] = error "Empty Move set" -- this should really not happen
lastMove x = head x

-- given a move and a replying utterance makes a new move referring to that
-- (it assumes the moving player is replying to the opponent's last move)
replyLastMoveFromOpponent :: Move -> CL -> Move
replyLastMoveFromOpponent m u = Move { ident = (ident m) + 1, pl = otherPlayer (pl m), s = u, t = t m}

containsMove :: Move -> Moves -> Bool
containsMove = elem

emptyMoves :: Moves -> Bool  
emptyMoves m = m == [DummyMove]

startingMoves :: Moves
startingMoves = [DummyMove]

insertMove :: Move -> Moves -> Moves
insertMove = (:)
