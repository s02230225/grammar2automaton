-- Determinize.hs
module Determinize
    ( isDeterministic
    , nfaToDFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sort)
import Automaton

isDeterministic :: NFA -> Bool
isDeterministic nfa = all ((<= 1) . Set.size) (Map.elems $ nfaTransitions nfa)

nfaToDFA :: NFA -> DFA
nfaToDFA nfa =
    let alphabet = Set.toList $ Set.map snd $ Map.keysSet $ nfaTransitions nfa
        
        transition states sym = Set.unions
            [ Map.findWithDefault Set.empty (st, sym) (nfaTransitions nfa)
            | st <- Set.toList states
            ]
        
        setName s = case Set.toList s of
            [] -> State "{}"
            [single] -> single
            multiple -> State $ "{" ++ intercalate "," (sort $ map (\(State str) -> str) multiple) ++ "}"
        
        startSet = Set.singleton (nfaStart nfa)
        startName = setName startSet
        
        build processed [] trans = (processed, trans)
        build processed ((set, name) : queue) trans =
            let newTrans = 
                    [ ((name, sym), setName nextSet)
                    | sym <- alphabet
                    , let nextSet = transition set sym
                    , not (Set.null nextSet)
                    ]
                newMappings = Map.fromList newTrans
                allTrans = Map.union newMappings trans
                nextSets = [ (nextSet, setName nextSet)
                           | (_, nextSet) <- map (\((_, s), n) -> (s, readSet n)) newTrans
                           , not (nextSet `Set.member` processed)
                           ]
                newProcessed = Set.insert set processed
                newQueue = queue ++ nextSets
            in build newProcessed newQueue allTrans
        
        readSet (State s) = 
            case s of
                '{' : rest ->
                    case reverse rest of
                        '}' : revBody -> Set.fromList $ map State $ splitOnComma $ reverse revBody
                        _ -> Set.singleton (State s)
                _ -> Set.singleton (State s)
        
        splitOnComma str = case break (== ',') str of
            (tok, "") -> [tok]
            (tok, _:rest) -> tok : splitOnComma rest
        
        (allSets, transitions) = build Set.empty [(startSet, startName)] Map.empty
        
        acceptDFA = Set.fromList
            [ name
            | set <- Set.toList allSets
            , let name = setName set
            , not (Set.null (Set.intersection set (nfaAccept nfa)))
            ]
        
        statesDFA = Set.fromList $ map setName (Set.toList allSets)
    in DFA
        { dfaStates = statesDFA
        , dfaStart = startName
        , dfaAccept = acceptDFA
        , dfaTransitions = transitions
        }