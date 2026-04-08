module Determinize
    ( isDeterministic
    , nfaToDFA
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sort)
import Automaton

-- | Проверяет, является ли НКА детерминированным.
--   Автомат детерминирован, если для любой пары (состояние, символ) множество целевых
--   состояний содержит не более одного элемента.
isDeterministic :: NFA -> Bool
isDeterministic nfa = all ((<= 1) . Set.size) (Map.elems $ nfaTransitions nfa)

-- | Преобразует НКА в эквивалентный ДКА методом построения подмножеств.
--   Каждое состояние ДКА соответствует множеству состояний НКА.
--   Имена состояний-множеств: если один элемент – его имя, иначе "{A,B,...}".
nfaToDFA :: NFA -> DFA
nfaToDFA nfa =
    let -- Алфавит – все символы, используемые в переходах
        alphabet = Set.toList $ Set.map snd $ Map.keysSet $ nfaTransitions nfa
        
        -- Функция перехода для множества состояний НКА
        transition :: Set.Set State -> Symbol -> Set.Set State
        transition states sym = Set.unions
            [ Map.findWithDefault Set.empty (st, sym) (nfaTransitions nfa)
            | st <- Set.toList states
            ]
        
        -- Генерация имени состояния ДКА по множеству состояний НКА
        setName :: Set.Set State -> State
        setName s = case Set.toList s of
            [] -> State "{}"
            [single] -> single                      -- одиночное состояние сохраняет исходное имя
            multiple -> State $ "{" ++ intercalate "," (sort $ map (\(State str) -> str) multiple) ++ "}"
        
        -- Начальное множество – { startState НКА }
        startSet = Set.singleton (nfaStart nfa)
        startName = setName startSet
        
        -- Рекурсивное построение всех достижимых множеств и переходов ДКА
        build :: Set.Set (Set.Set State)               -- уже обработанные множества
              -> [(Set.Set State, State)]              -- очередь (множество, его имя)
              -> Map.Map (State, Symbol) State         -- накопленные переходы
              -> (Set.Set (Set.Set State), Map.Map (State, Symbol) State)
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
                -- Новые множества для обработки (если ещё не были обработаны)
                nextSets = [ (nextSet, setName nextSet)
                           | (_, nextSet) <- map (\((_, s), n) -> (s, readSet n)) newTrans
                           , not (nextSet `Set.member` processed)
                           ]
                newProcessed = Set.insert set processed
                newQueue = queue ++ nextSets
            in build newProcessed newQueue allTrans
        
        -- Обратное преобразование имени состояния ДКА во множество состояний НКА
        readSet :: State -> Set.Set State
        readSet (State s) = 
            case s of
                '{' : rest ->  -- если имя начинается с '{', это множество
                    case reverse rest of
                        '}' : revBody -> Set.fromList $ map State $ splitOnComma $ reverse revBody
                        _ -> Set.singleton (State s)   -- на случай ошибки формата
                _ -> Set.singleton (State s)
        
        -- Разделение строки по запятой (для парсинга "{A,B}")
        splitOnComma :: String -> [String]
        splitOnComma str = case break (== ',') str of
            (tok, "") -> [tok]
            (tok, _:rest) -> tok : splitOnComma rest
        
        (allSets, transitions) = build Set.empty [(startSet, startName)] Map.empty
        
        -- Финальные состояния ДКА – множества, содержащие хотя бы одно финальное состояние НКА
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