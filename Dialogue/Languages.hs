module Dialogue.Languages where

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


data PlayerRole = P | O deriving (Show, Eq)

otherPlayer :: PlayerRole -> PlayerRole
otherPlayer P = O
otherPlayer O = P



