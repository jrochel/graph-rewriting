-- | Syntax does not allow spaces, combinator names in capital letters, any other character besides combinators and parentheses is interpreted as a variable name. 

module Common.Term where

import Prelude.Unicode
import Text.ParserCombinators.Parsec as Parsec
import Control.Monad (liftM)

data Expr = A Expr Expr | S | K | I | V String deriving (Ord, Eq)

instance Show Expr where
	show = showComp False where
		showComp :: Bool → Expr → String
		showComp isComponent expr = case expr of
			A (A e1 e2) e3 → maybeWrap $ show e1 ⧺ " " ⧺ diverge e2 ⧺ " " ⧺ diverge e3
			A e1 e2 → maybeWrap $ diverge e1 ⧺ " " ⧺ diverge e2
			S → "S"
			K → "K"
			I → "I"
			V v → v
			where
			maybeWrap str = if isComponent then "(" ⧺ str ⧺ ")" else str
			diverge = showComp True

parse ∷ String → Expr
parse str = either (error ∘ show) id (Parsec.parse parser "(null)" str)

parseFile ∷ FilePath → IO Expr
parseFile = liftM (either (error ∘ show) id) ∘ Parsec.parseFromFile parser

parser ∷ Parser Expr
parser = let expr = parser in liftM (foldl1 A) $ many1 $ choice
	[char 'S' >> return S,
	 char 'K' >> return K,
	 char 'I' >> return I,
	 between (char '(') (char ')') expr,
	 liftM (V ∘ return) letter]
