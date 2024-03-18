{-# LANGUAGE UnicodeSyntax #-}
module Term where

import Prelude.Unicode
import Text.Parsec
import Control.Monad.Identity
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import Text.Parsec.IndentParsec

-- | The AST of a lambda expression
data Λ = A Λ Λ            -- ^ application
	| Λ String Λ           -- ^ abstraction
	| L [(String,Λ)] Λ     -- ^ let binding
	| Case Λ [(Pattern,Λ)] -- ^ case expression
	| C String [Λ]         -- ^ constructor
	deriving (Show,Eq,Ord)

-- | The LHS of a case expression. Numbers are parsed as strings.
data Pattern = Pat {constr ∷ String, vars ∷ [String]} deriving (Show, Eq, Ord)

type IndentCharParser a = IndentParsec String () a

langDef = LanguageDef { commentStart = "{-"
                      , commentEnd   = "-}"
                      , commentLine  = "--"
                      , identStart = letter   <|> char '_'
                      , identLetter = alphaNum <|> char '_'
                      , opStart = oneOf "-+/*=<>"
                      , opLetter = oneOf "-+/*=<>"
                      , reservedNames = ["let", "in", "case", "of"]
                      , reservedOpNames = ["=", "->", "→", "."]
                      , caseSensitive = False
                      , nestedComments = True
                      }

parseFile ∷ FilePath → IO Λ
parseFile f = do
	file ← readFile f
	let parseErrorOrExpr = runIdentity $ runGIPT expression () f file
	return $ either (error ∘ show) id parseErrorOrExpr

expression ∷ IndentCharParser Λ
expression = flip label "expression" $ letBinding <|> caseExpr <|> application

application ∷ IndentCharParser Λ
application = foldl1 A <$> many1 (parenthetic <|> abstraction <|> variable <|> numeral)

variable ∷ IndentCharParser Λ
variable = C <$> (ident <|> operator tokP) <*> pure []

numeral ∷ IndentCharParser Λ
numeral = C <$> numeric <*> pure []

numeric ∷ IndentCharParser String
numeric = either show show <$> naturalOrFloat tokP

parenthetic ∷ IndentCharParser Λ
parenthetic = parens tokP expression

tokP :: IndentTokenParser String () Identity
tokP = makeTokenParser langDef

caseExpr ∷ IndentCharParser Λ
caseExpr = flip label "case expression" $ do
	keyword "case"
	Case <$> expression <* keyword "of" <*> blockOf (many1 $ foldedLinesOf pattern)

arrow ∷ IndentCharParser ()
arrow = reservedOp tokP "→" <|> reservedOp tokP "->"

point ∷ IndentCharParser ()
point = reservedOp tokP "."

pattern ∷ IndentCharParser (Pattern, Λ)
pattern = (,) <$> lhs <*> expression where
	lhs = Pat <$> (ident <|> numeric) <*> manyTill (ident <|> numeric) arrow

lambda ∷ IndentCharParser String
lambda = sym "λ" <|> sym "\\"

abstraction ∷ IndentCharParser Λ
abstraction = flip label "abstraction" $
	flip (foldr Λ) <$> (lambda *> many1 ident) <*> ((arrow <|> point) *> expression)

letBinding ∷ IndentCharParser Λ
letBinding = flip label "let binding" $ do
	keyword "let"
	try oneLiner <|> multipleBindings where
		oneLiner = do
			b ← binding
			L [b] <$> (keyword "in" *> expression)
		multipleBindings = L <$> blockOf (many1 $ foldedLinesOf binding) <*> (keyword "in" *> expression)

binding ∷ IndentCharParser (String,Λ)
binding = flip label "binding" $ do
	funct ← ident
	rhs ← flip (foldr Λ) <$> manyTill ident equals <*> expression
	return (funct, rhs)

keyword ∷ String → IndentCharParser ()
keyword = reserved tokP

ident ∷ IndentCharParser String
ident = identifier tokP

sym ∷ String → IndentCharParser String
sym = symbol tokP

equals ∷ IndentCharParser ()
equals = reservedOp tokP "="
