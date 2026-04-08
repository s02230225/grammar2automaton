module Convert
    ( grammarToNFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grammar
import Automaton

-- | Строит НКА по регулярной грамматике заданного типа.
--   Алгоритм стандартный:
--   - Праволинейная: начальное состояние H, финальное #.
--     Правила A -> a дают переход в #, A -> aB – в B.
--   - Леволинейная: начальное состояние @, финальные – нетерминалы с пустыми правилами.
--     Правила A -> a дают переход из @ в A, A -> Ba – из B в A.
grammarToNFA :: Grammar -> GrammarType -> NFA
grammarToNFA grammar gtype =
    case gtype of
        RightLinear -> buildRightLinear
        LeftLinear  -> buildLeftLinear
  where
    -- Создаёт состояние из нетерминала (один символ)
    stateFromNT :: NonTerminal -> State
    stateFromNT (NonTerminal c) = State [c]

    buildRightLinear :: NFA
    buildRightLinear =
        let startState = State "H"
            finalState = State "#"
            ntStates = Set.map stateFromNT (Map.keysSet grammar)
            allStates = Set.insert finalState ntStates
            trans = Map.fromListWith Set.union $
                -- Терминальные правила A -> a : переход A -a-> #
                [ ((stateFromNT nt, Symbol t), Set.singleton finalState)
                | (nt, alts) <- Map.toList grammar
                , TerminalOnly (Terminal t) <- alts
                ] ++
                -- Праволинейные правила A -> aB : переход A -a-> B
                [ ((stateFromNT nt, Symbol t), Set.singleton (stateFromNT nt2))
                | (nt, alts) <- Map.toList grammar
                , RightLinearAlt (Terminal t) nt2 <- alts
                ]
        in NFA
            { nfaStates = allStates
            , nfaStart = startState
            , nfaAccept = Set.singleton finalState
            , nfaTransitions = trans
            }

    buildLeftLinear :: NFA
    buildLeftLinear =
        let startState = State "@"
            ntStates = Set.map stateFromNT (Map.keysSet grammar)
            -- Финальные состояния – нетерминалы с пустыми альтернативами (отсутствие правил)
            acceptStates = Set.fromList
                [ stateFromNT nt
                | nt <- Map.keys grammar
                , null (grammar Map.! nt)
                ]
            allStates = Set.insert startState ntStates
            trans = Map.fromListWith Set.union $
                -- Терминальные правила A -> a : переход @ -a-> A
                [ ((startState, Symbol t), Set.singleton (stateFromNT nt))
                | (nt, alts) <- Map.toList grammar
                , TerminalOnly (Terminal t) <- alts
                ] ++
                -- Леволинейные правила A -> Ba : переход B -a-> A
                [ ((stateFromNT nt1, Symbol t), Set.singleton (stateFromNT nt2))
                | (nt2, alts) <- Map.toList grammar
                , LeftLinearAlt nt1 (Terminal t) <- alts
                ]
        in NFA
            { nfaStates = allStates
            , nfaStart = startState
            , nfaAccept = acceptStates
            , nfaTransitions = trans
            }