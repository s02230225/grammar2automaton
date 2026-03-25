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


-- Data types ---------------------------------------------------------

newtype NonTerminal = NonTerminal Char deriving (Eq, Ord, Show)
newtype Terminal = Terminal Char deriving (Eq, Ord, Show)

data Alternative
    = TerminalOnly Terminal
    | RightLinearAlt Terminal NonTerminal
    | LeftLinearAlt NonTerminal Terminal
    deriving (Eq, Show)

type Grammar = Map.Map NonTerminal [Alternative]

data GrammarType
    = RightLinear
    | LeftLinear
    deriving (Eq, Show)

-- Чистые функции ----------------------------------------------------

parseGrammar :: String -> Grammar
parseGrammar str = undefined

getGrammarType :: Grammar -> GrammarType
getGrammarType grammar = undefined

validateGrammar :: Grammar -> GrammarType -> Either String ()
validateGrammar grammar gtype = undefined
