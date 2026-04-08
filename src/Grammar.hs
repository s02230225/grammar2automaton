module Grammar
    ( NonTerminal(..)
    , Terminal(..)
    , Alternative(..)
    , Grammar
    , GrammarType(..)
    , parseGrammar
    , getGrammarType
    , validateGrammar
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.Char (isUpper, isLower)
import Control.Monad (unless)

-- Типы данных --------------------------------------------------------------

-- | Нетерминальный символ (заглавная латинская буква)
newtype NonTerminal = NonTerminal Char deriving (Eq, Ord, Show)

-- | Терминальный символ (строчная латинская буква)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)

-- | Альтернатива в правой части правила грамматики
data Alternative
    = TerminalOnly Terminal                -- ^ только терминал (a)
    | RightLinearAlt Terminal NonTerminal  -- ^ терминал + нетерминал (aN) – праволинейная
    | LeftLinearAlt NonTerminal Terminal   -- ^ нетерминал + терминал (Na) – леволинейная
    deriving (Eq, Show)

-- | Грамматика – отображение нетерминала в список альтернатив
type Grammar = Map.Map NonTerminal [Alternative]

-- | Тип грамматики (праволинейная или леволинейная)
data GrammarType
    = RightLinear
    | LeftLinear
    deriving (Eq, Show)

-- Вспомогательные функции ---------------------------------------------------

-- | Удаляет все пробелы из строки (для упрощения парсинга)
trim :: String -> String
trim = filter (/= ' ')

-- | Пытается распарсить строку как одну альтернативу
parseAlternative :: String -> Maybe Alternative
parseAlternative alt = case alt of
    [c] | isLower c -> Just $ TerminalOnly (Terminal c)
    [c1, c2]
        | isLower c1 && isUpper c2 -> Just $ RightLinearAlt (Terminal c1) (NonTerminal c2)
        | isUpper c1 && isLower c2 -> Just $ LeftLinearAlt (NonTerminal c1) (Terminal c2)
    _ -> Nothing

-- | Парсит строку с грамматикой, разделённую ';', в структуру Grammar.
--   При ошибке вызывает error (можно заменить на Either для более безопасной обработки).
parseGrammar :: String -> Grammar
parseGrammar str = Map.fromList rules
  where
    trimmed = trim str
    ruleStrs = filter (not . null) $ splitOn ";" trimmed
    parseRule :: String -> (NonTerminal, [Alternative])
    parseRule rs =
        let (left, rightRaw) = break (== '=') rs
            -- Безопасное извлечение первого символа через pattern matching
            leftNT = case left of
                [c] | isUpper c -> NonTerminal c
                _ -> error $ "Invalid nonterminal: " ++ left
            rightStr = drop 1 rightRaw  -- убираем '='
            alts = if null rightStr
                      then []
                      else map trim $ splitOn "|" rightStr
            parsedAlts = map parseAlternative alts
        in if any (== Nothing) parsedAlts
              then error $ "Invalid alternative in rule: " ++ rs
              else (leftNT, map (\(Just a) -> a) parsedAlts)
    rules = map parseRule ruleStrs

-- | Определяет тип грамматики по наличию характерных альтернатив
getGrammarType :: Grammar -> GrammarType
getGrammarType grammar =
    let allAlts = concat $ Map.elems grammar
        hasRight = any isRightLinear allAlts
        hasLeft  = any isLeftLinear allAlts
    in case (hasRight, hasLeft) of
        (True, False) -> RightLinear
        (False, True) -> LeftLinear
        (False, False) -> RightLinear   -- только TerminalOnly считаем праволинейной
        (True, True)  -> error "Mixed alternatives detected"

isRightLinear :: Alternative -> Bool
isRightLinear (RightLinearAlt _ _) = True
isRightLinear _ = False

isLeftLinear :: Alternative -> Bool
isLeftLinear (LeftLinearAlt _ _) = True
isLeftLinear _ = False

-- | Проверяет корректность грамматики:
--   - все альтернативы соответствуют заявленному типу,
--   - все нетерминалы в правых частях определены,
--   - пустые альтернативы допустимы только в леволинейной грамматике.
validateGrammar :: Grammar -> GrammarType -> Either String ()
validateGrammar grammar gtype = do
    -- 1. Проверка типов альтернатив
    let invalidAlts = filter (not . matchesType gtype) (concat $ Map.elems grammar)
    unless (null invalidAlts) $
        Left $ "Alternatives do not match grammar type: " ++ show invalidAlts

    -- 2. Проверка определённости всех нетерминалов в правых частях
    let defined = Map.keysSet grammar
        used = Set.unions $ map alternativesUsed (concat $ Map.elems grammar)
        undefinedNTs = Set.difference used defined
    unless (Set.null undefinedNTs) $
        Left $ "Undefined nonterminals: " ++ show (Set.toList undefinedNTs)

    -- 3. Пустые альтернативы разрешены только в леволинейной грамматике
    let emptyNTs = [nt | (nt, alts) <- Map.toList grammar, null alts]
    unless (null emptyNTs || gtype == LeftLinear) $
        Left $ "Empty alternatives only allowed in left-linear grammar, found for: " ++ show emptyNTs

    return ()
  where
    matchesType :: GrammarType -> Alternative -> Bool
    matchesType RightLinear (RightLinearAlt _ _) = True
    matchesType RightLinear (TerminalOnly _)     = True
    matchesType LeftLinear  (LeftLinearAlt _ _)  = True
    matchesType LeftLinear  (TerminalOnly _)     = True
    matchesType _ _ = False

    alternativesUsed :: Alternative -> Set.Set NonTerminal
    alternativesUsed (RightLinearAlt _ nt) = Set.singleton nt
    alternativesUsed (LeftLinearAlt nt _)  = Set.singleton nt
    alternativesUsed (TerminalOnly _)      = Set.empty