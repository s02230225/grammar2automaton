module Automaton
    ( State(..)
    , Symbol(..)
    , NFA(..)
    , DFA(..)
    , Edge
    , nfaToEdges
    , dfaToEdges
    , edgesToString
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar (NonTerminal, Terminal)

-- Data types -------------------------------------------------------------

newtype State = State Char deriving (Eq, Ord, Show)
newtype Symbol = Symbol Char deriving (Eq, Ord, Show)

data NFA = NFA
    { nfaStates      :: Set.Set State
    , nfaStart       :: State
    , nfaAccept      :: Set.Set State
    , nfaTransitions :: Map.Map (State, Symbol) (Set.Set State)
    } deriving (Show)

data DFA = DFA
    { dfaStates      :: Set.Set State
    , dfaStart       :: State
    , dfaAccept      :: Set.Set State
    , dfaTransitions :: Map.Map (State, Symbol) State
    } deriving (Show)

type Edge = (State, Symbol, State)

-- Чистые функции -----------------------------------------------------

nfaToEdges :: NFA -> [Edge]
nfaToEdges nfa = undefined

dfaToEdges :: DFA -> [Edge]
dfaToEdges dfa = undefined

edgesToString :: [Edge] -> String
edgesToString edges = undefined
