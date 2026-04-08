module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate, sort)

import Grammar
import Automaton
import Convert
import Determinize

-- | Главная функция: читает входную грамматику, выполняет все этапы обработки
--   и выводит результат в требуемом формате.
main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents                 -- читаем из stdin
        (file:_) -> readFile file         -- или из файла, переданного аргументом
    let trimmed = filter (/= '\n') input  -- удаляем переводы строк для чистоты вывода
    putStrLn "################################################################################"
    putStrLn $ "[1] Входная грамматика:"
    putStrLn $ "    \"" ++ trimmed ++ "\""
    
    -- Парсинг и валидация
    let grammar = parseGrammar trimmed
    let gtype = getGrammarType grammar
    case validateGrammar grammar gtype of
        Left err -> do
            hPutStrLn stderr $ "Ошибка валидации: " ++ err
        Right () -> do
            putStrLn ""
            putStrLn "[2] Результат парсинга:"
            putStrLn $ "    Тип: " ++ show gtype
            let startSym = case gtype of
                                RightLinear -> 'H'
                                LeftLinear  -> 'S'
            putStrLn $ "    Начальный символ: " ++ [startSym]
            putStrLn "    Продукции:"
            mapM_ (putStrLn . ("        " ++)) (formatProductions grammar)
            
            -- Построение НКА
            let nfa = grammarToNFA grammar gtype
            putStrLn ""
            putStrLn "[3] Недетерминированный конечный автомат (НКА):"
            putStrLn $ "    Начальное состояние: " ++ show (nfaStart nfa)
            putStrLn $ "    Финальные состояния: " ++ formatStateSet (nfaAccept nfa)
            putStrLn "    Переходы:"
            mapM_ (putStrLn . ("        " ++)) (formatNFATransitions nfa)
            
            -- Проверка детерминированности
            let det = isDeterministic nfa
            putStrLn ""
            putStrLn $ "[4] Проверка детерминированности: " ++ if det then "ДА" else "НЕТ (требуется детерминизация)"
            
            -- Детерминизация (всегда строим ДКА, метод подмножеств)
            let dfa = nfaToDFA nfa
            let resultEdges = dfaToEdges dfa
            
            putStrLn ""
            putStrLn "[5] Детерминированный конечный автомат (ДКА):"
            putStrLn $ "    Начальное состояние: " ++ show (dfaStart dfa)
            putStrLn $ "    Финальные состояния:"
            mapM_ (putStrLn . ("        " ++)) (map show $ Set.toList $ dfaAccept dfa)
            putStrLn "    Переходы:"
            mapM_ (putStrLn . ("        " ++)) (formatDFATransitions dfa)
            
            putStrLn ""
            putStrLn "[6] Результат (список рёбер):"
            putStr $ edgesToString resultEdges
            putStrLn "################################################################################"

-- Вспомогательные функции форматирования для вывода -------------------------

-- | Форматирует продукции грамматики для вывода
formatProductions :: Grammar -> [String]
formatProductions grammar =
    [ formatNT nt ++ " -> " ++ intercalate " | " (map formatAlt alts)
    | (nt, alts) <- Map.toList grammar
    ]
  where
    formatNT (NonTerminal c) = [c]
    formatAlt (TerminalOnly (Terminal t)) = [t]
    formatAlt (RightLinearAlt (Terminal t) (NonTerminal nt)) = [t] ++ [nt]
    formatAlt (LeftLinearAlt (NonTerminal nt) (Terminal t)) = [nt] ++ [t]

-- | Форматирует множество состояний в строку вида "{A,B}"
formatStateSet :: Set.Set State -> String
formatStateSet s = "{" ++ intercalate "," (map show (Set.toList s)) ++ "}"

-- | Форматирует переходы НКА для вывода (с множествами целевых состояний)
formatNFATransitions :: NFA -> [String]
formatNFATransitions nfa =
    [ show from ++ " -" ++ [c] ++ "-> " ++ formatStateSet toSet
    | ((from, Symbol c), toSet) <- Map.toList (nfaTransitions nfa)
    ]

-- | Форматирует переходы ДКА для вывода
formatDFATransitions :: DFA -> [String]
formatDFATransitions dfa =
    [ show from ++ " -" ++ [c] ++ "-> " ++ show to
    | ((from, Symbol c), to) <- Map.toList (dfaTransitions dfa)
    ]