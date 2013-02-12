
-- Agents
type KB = [LL]

data DialogueAgent = Agent {pl :: Player, -- role of the agent in the dialogue
                            kb :: KB, -- static original KB
                            drb :: KB }
                            

-- Policies 
policy :: Agent -> Move -> 
