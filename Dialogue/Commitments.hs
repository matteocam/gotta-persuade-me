module Dialogue.Commitments where

import Dialogue.Languages
import Dialogue.Move

import Data.List
import Control.Arrow

type CommitmentStore = [LL]
type CS = CommitmentStore
type CSs = (CS, CS)

getCS :: PlayerRole -> CSs -> CS
getCS P = fst 
getCs O = snd

emptyCS = []
emptyCSs = (emptyCS, emptyCS)

type CSUpdateFun = CS -> CS

addCommitment :: LL -> CSUpdateFun
addCommitment p cs = [p] `union` cs

addCommitments :: [LL] -> CSUpdateFun
addCommitments = union

removeCommitment :: LL -> CSUpdateFun
removeCommitment = delete


-- takes a speech act and returns a commitment update function
updateCS :: CL -> CSUpdateFun
updateCS (Claim p) = addCommitment p
updateCS (Why _)  = id
updateCS (Since _ ps) = addCommitments ps  -- XXX: the conclusion is not in the new commitment store
updateCS (Retract p) = removeCommitment p 

-- allows working on the pair of commitments dicrectly
updateCSFromRole P = first
updateCSFromRole O = second

-- takes a player 
updatePlayerCS :: PlayerRole -> Move -> CSs -> CSs
updatePlayerCS role move css = let utterance = s move
                                   selectorArw = updateCSFromRole role
                               in selectorArw (updateCS utterance) css 


