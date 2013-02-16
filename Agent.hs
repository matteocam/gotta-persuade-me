module Agent where

import Dialogue.Languages
import Dialogue.Move
import Dialogue.Protocol

-- Agents
type KB = [LL]

data DialogueAgent = Agent {role :: PlayerRole, -- role of the agent in the dialogue
                            kb :: KB, -- static original KB
                            drb :: KB } deriving Show
                            
instance Player DialogueAgent where
  makeDialogueMove _ pl = Move 0 (Claim "p") (role pl) 0
  
{- 
Experimenting with the "architecture" on different layer.

Definition of reasoning:
assuming a certain semantics, reasoning from an Argumentation Theory AT on a
proposition phi is to give an assignment phi which is Overruled, Acceptable, Justified. 

Again, this thing may work like this:
- agents REASON with a certain DRB, having a function 
     reason :: KB -> LL -> DialogicalStatus
     reason drb p = statusInAT p argTheory(drb)
     acceptableStatements :: KB -> KB
     acceptableStatements = filter_all_acceptable_from_possible_queries 

- agents have ATTITUDES. Attitudes are heuristics, a very nice one, they basically say what the agents should do with what they have
  what you are going to utter.
  Example of an attitude: an agent can only spit out what he has a justified argument for.
     allowedPropositionsFromAttitude :: KB -> KB

But yeah, how is an agent guided? Do they have higher level directions (destroy and build?) 
The other part of the question is, what do the attitudes return: speech acts / propositions? Maybe they are more complex than a simple 
filter after all, what do you think?

-}
  
  
-- example

-- Policies 
-- policy :: Agent -> Move -> 
