module TestDialogue where

import Test.QuickCheck

import Dialogue.Protocol
import Dialogue.Languages
import Dialogue.Move
import Agent

-- Example Dialogue
exampleDialogue = emptyDialogue { topic = t, moves = reverse rmoves }
  where t = "p" 
        rmoves = [DummyMove,
                  Move 1 (Claim "p")  P 0,
                  Move 2 (Claim "not(p)") O 1,
                  Move 3 (Claim "concede not(p)") P 2]

              
exampleDialogue1 = exampleDialogue {moves = tail (moves exampleDialogue)}


-- Example Agent
data StupidArguingAgent = SAAgent {rl :: PlayerRole} deriving Show

instance Player StupidArguingAgent where 
  makeDialogueMove d pl  = case (s lu) of
                                Claim p -> replyLastMoveFromOpponent lu (Why p)
                                Why p -> replyLastMoveFromOpponent lu (p `Since` [p])
    where lu = lastUtterance d (otherPlayer $ rl pl)

stupid_p = SAAgent P
stupid_o = SAAgent O

-- Tests

-- simple stupid agent test
prop_saa = (makeDialogueMove dlg stupid_p) == expectedMove
  where dlg = exampleDialogue1 {proponent = stupid_p, opponent = stupid_o}
        expectedMove =  Move {ident = 3, s = Why "not(p)", pl = P, t = 1}

test_all = quickCheck prop_saa 