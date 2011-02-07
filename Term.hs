{-# LANGUAGE UnicodeSyntax, FlexibleContexts, Rank2Types #-}
module Term where

import Prelude.Unicode
import Text.ParserCombinators.UU as UU
import Text.ParserCombinators.UU.Utils as UU
import Text.ParserCombinators.UU.BasicInstances as UU


data Term = App Term Term | Var Char deriving (Ord, Eq)

type Import = FilePath

instance Show Term where
	show = showComp False where
		showComp :: Bool → Term → String
		showComp isComponent expr = case expr of
			App (App e1 e2) e3 → maybeWrap $ show e1 ⧺ diverge e2 ⧺ diverge e3
			App e1 e2 → maybeWrap $ diverge e1 ⧺ diverge e2
			Var v → [v]
			where
			maybeWrap str = if isComponent then "(" ⧺ str ⧺ ")" else str
			diverge = showComp True

parse ∷ Parser a → String → (a, [Error LineCol])
parse p inp = UU.parse ((,) <$> p <*> pEnd) (createStr (LineCol 0 0) inp)

var ∷ Parser Term
var = Var <$> alphaNum <?> "variable"

term ∷ Parser Term
term = foldl1 App <$> pList1 (var <|> parens term) <?> "term"
	where parens p = pSym '(' *> p <* pSym ')'

pImport ∷ Parser Import
pImport = pToken "import" *>ε*> pList1 asciiPrintable

ruleset ∷ Parser ([Import], [(Term,Term)])
ruleset = (,) <$> pList (pImport <* newlines) <*> pList1 (rule <* newlines)

rule ∷ Parser (Term,Term)
rule = (,) <$> term <*ε<* arrow <*ε<*> term

-- primitive parsers

newlines ∷ Parser ()
newlines = pMunch (`elem` "\n\r") *> pure ()

ε ∷ Parser () -- spaces
ε = pMunch (`elem` "\t ") *> pure ()

arrow ∷ Parser String
arrow = pToken "->" <|> pToken "→" <?> "arrow"

alphaNum ∷ Parser Char
alphaNum = pLetter <|> pDigit <?> "alphaNum"

asciiPrintable ∷ Parser Char
asciiPrintable = pRange ('!', '~')
