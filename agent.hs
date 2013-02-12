module Agent where

import Dialogue 

-- Agents
type KB = [LL]

data DialogueAgent = Agent {role :: PlayerRole, -- role of the agent in the dialogue
                            kb :: KB, -- static original KB
                            drb :: KB }
                            
instance Player DialogueAgent where
  makeDialogueMove pl = Move 0 (Claim "p") (role pl) 0

-- Policies 
-- policy :: Agent -> Move -> 
