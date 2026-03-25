module Convert
    ( grammarToNFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar
import Automaton

grammarToNFA :: Grammar -> GrammarType -> NFA
grammarToNFA grammar gtype = undefined
