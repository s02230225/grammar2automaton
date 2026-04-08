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

-- Типы данных --------------------------------------------------------------

-- | Состояние автомата. Используется String для поддержки имён вида "{A,B}" при детерминизации.
newtype State = State String deriving (Eq, Ord)

instance Show State where
    show (State s) = s   -- при выводе опускаем конструктор

-- | Символ алфавита (терминал)
newtype Symbol = Symbol Char deriving (Eq, Ord, Show)

-- | Недетерминированный конечный автомат (НКА)
data NFA = NFA
    { nfaStates      :: Set.Set State            -- ^ множество всех состояний
    , nfaStart       :: State                    -- ^ начальное состояние
    , nfaAccept      :: Set.Set State            -- ^ множество допускающих состояний
    , nfaTransitions :: Map.Map (State, Symbol) (Set.Set State)  -- ^ функция переходов
    } deriving (Show)

-- | Детерминированный конечный автомат (ДКА)
data DFA = DFA
    { dfaStates      :: Set.Set State            -- ^ множество состояний
    , dfaStart       :: State                    -- ^ начальное состояние
    , dfaAccept      :: Set.Set State            -- ^ допускающие состояния
    , dfaTransitions :: Map.Map (State, Symbol) State  -- ^ функция переходов (один целевой состояние)
    } deriving (Show)

-- | Ребро графа переходов: (исходное состояние, символ, целевое состояние)
type Edge = (State, Symbol, State)

-- Чистые функции ---------------------------------------------------------

-- | Преобразует НКА в список рёбер (раскрывая множества целевых состояний)
nfaToEdges :: NFA -> [Edge]
nfaToEdges nfa = 
    [ (from, sym, to)
    | ((from, sym), toSet) <- Map.toList (nfaTransitions nfa)
    , to <- Set.toList toSet
    ]

-- | Преобразует ДКА в список рёбер
dfaToEdges :: DFA -> [Edge]
dfaToEdges dfa =
    [ (from, sym, to)
    | ((from, sym), to) <- Map.toList (dfaTransitions dfa)
    ]

-- | Форматирует список рёбер в строку, каждое ребро на отдельной строке.
--   Формат: from -> sym -> to
edgesToString :: [Edge] -> String
edgesToString = unlines . map formatEdge
  where
    formatEdge (from, Symbol sym, to) = show from ++ " -> " ++ [sym] ++ " -> " ++ show to