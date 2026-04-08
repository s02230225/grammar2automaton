-- Grammar.hs
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

trim :: String -> String
trim = filter (/= ' ')

parseAlternative :: String -> Maybe Alternative
parseAlternative alt = case alt of
    [c] | isLower c -> Just $ TerminalOnly (Terminal c)
    [c1, c2]
        | isLower c1 && isUpper c2 -> Just $ RightLinearAlt (Terminal c1) (NonTerminal c2)
        | isUpper c1 && isLower c2 -> Just $ LeftLinearAlt (NonTerminal c1) (Terminal c2)
    _ -> Nothing

parseGrammar :: String -> Grammar
parseGrammar str = Map.fromList rules
  where
    trimmed = trim str
    ruleStrs = filter (not . null) $ splitOn ";" trimmed
    parseRule rs =
        let (left, rightRaw) = break (== '=') rs
            leftNT = case left of
                [c] | isUpper c -> NonTerminal c
                _ -> error $ "Invalid nonterminal: " ++ left
            rightStr = drop 1 rightRaw
            alts = case rightStr of
                "" -> []
                _  -> map trim $ splitOn "|" rightStr
            parsedAlts = map parseAlternative alts
        in case any (== Nothing) parsedAlts of
            True  -> error $ "Invalid alternative in rule: " ++ rs
            False -> (leftNT, map (\(Just a) -> a) parsedAlts)
    rules = map parseRule ruleStrs

getGrammarType :: Grammar -> GrammarType
getGrammarType grammar =
    let allAlts = concat $ Map.elems grammar
        hasRight = any isRightLinear allAlts
        hasLeft  = any isLeftLinear allAlts
    in case (hasRight, hasLeft) of
        (True, False) -> RightLinear
        (False, True) -> LeftLinear
        (False, False) -> RightLinear
        (True, True)  -> error "Mixed alternatives detected"

isRightLinear :: Alternative -> Bool
isRightLinear (RightLinearAlt _ _) = True
isRightLinear _ = False

isLeftLinear :: Alternative -> Bool
isLeftLinear (LeftLinearAlt _ _) = True
isLeftLinear _ = False

validateGrammar :: Grammar -> GrammarType -> Either String ()
validateGrammar grammar gtype = do
    let invalidAlts = filter (not . matchesType gtype) (concat $ Map.elems grammar)
    unless (null invalidAlts) $
        Left $ "Alternatives do not match grammar type: " ++ show invalidAlts

    let defined = Map.keysSet grammar
        used = Set.unions $ map alternativesUsed (concat $ Map.elems grammar)
        undefinedNTs = Set.difference used defined
    unless (Set.null undefinedNTs) $
        Left $ "Undefined nonterminals: " ++ show (Set.toList undefinedNTs)

    let emptyNTs = [nt | (nt, alts) <- Map.toList grammar, null alts]
    unless (null emptyNTs || gtype == LeftLinear) $
        Left $ "Empty alternatives only allowed in left-linear grammar, found for: " ++ show emptyNTs

    return ()
  where
    matchesType RightLinear (RightLinearAlt _ _) = True
    matchesType RightLinear (TerminalOnly _)     = True
    matchesType LeftLinear  (LeftLinearAlt _ _)  = True
    matchesType LeftLinear  (TerminalOnly _)     = True
    matchesType _ _ = False

    alternativesUsed (RightLinearAlt _ nt) = Set.singleton nt
    alternativesUsed (LeftLinearAlt nt _)  = Set.singleton nt
    alternativesUsed (TerminalOnly _)      = Set.empty