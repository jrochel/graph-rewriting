{-# LANGUAGE UnicodeSyntax #-}
module Term where

import Prelude.Unicode
import Text.ParserCombinators.Parsec as Parsec
	hiding (space, eof, notFollowedBy, anyChar, many, optional, (<|>))
import Text.ParserCombinators.Parsec.IndentParser as Indent
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.IndentParser.Token
import qualified Text.ParserCombinators.Parsec.Token as T
import Control.Monad
import Data.Functor
import Control.Applicative

-- | The AST of a lambda expression
data Λ = A Λ Λ            -- ^ application
	| Λ String Λ           -- ^ abstraction
	| L [(String,Λ)] Λ     -- ^ let binding
	| Case Λ [(Pattern,Λ)] -- ^ case expression
	| C String [Λ]         -- ^ constructor
	deriving (Show,Eq,Ord)

-- | The LHS of a case expression. Numbers are parsed as strings.
data Pattern = Pat {constr ∷ String, vars ∷ [String]} deriving (Show, Eq, Ord)

testParser ∷ IndentParser tok () c → [tok] → c
testParser parser str = either (error ∘ show) id (Indent.parse parser "(null)" str)

parse ∷ [Char] → Λ
parse = testParser expression

parseFile ∷ FilePath → IO Λ
parseFile = liftM (either (error ∘ show) id) ∘ Indent.parseFromFile expression

expression ∷ IndentCharParser st Λ
expression = flip label "expression" $ letBinding <|> caseExpr <|> application

application ∷ IndentCharParser st Λ
application = foldl1 A <$> many1 (parenthetic <|> abstraction <|> variable <|> numeral)

variable ∷ IndentCharParser st Λ
variable = C <$> (ident <|> operator haskell) <*> pure []

numeral ∷ IndentCharParser st Λ
numeral = C <$> numeric <*> pure []

numeric ∷ IndentCharParser st String
numeric = either show show <$> naturalOrFloat haskell

parenthetic ∷ IndentCharParser st Λ
parenthetic = parens haskell expression

tokP ∷ T.TokenParser st
tokP = T.makeTokenParser haskellDef

caseExpr ∷ IndentCharParser st Λ
caseExpr = flip label "case expression" $
	Case <$> (keyword "case" *> expression <* keyword "of") <*> bracesOrBlock tokP patterns

patterns ∷ IndentCharParser st [(Pattern, Λ)]
patterns = semiOrNewLineSep tokP pattern

arrow ∷ IndentCharParser st String
arrow = sym "→" <|> sym "->"

pattern ∷ IndentCharParser st (Pattern, Λ)
pattern = (,) <$> lhs <*> (arrow *> expression) where
	lhs = Pat <$> (ident <|> numeric) <*> many (ident <|> numeric)

lambda ∷ IndentCharParser st String
lambda = sym "λ" <|> sym "\\"

abstraction ∷ IndentCharParser st Λ
abstraction = flip label "abstraction" $
	flip (foldr Λ) <$> (lambda *> many1 ident) <*> ((sym "." <|> arrow) *> expression)

-- Ugly, but works. Keyword "in" terminates binding blocks and bindings. Allows empty lets
letBinding ∷ IndentCharParser st Λ
letBinding = flip label "let binding" $ do
	let parseBindings = do
		e ← optionMaybe $ keyword "in" *> expression
		case e of
			Just je → return ([], Just je)
			Nothing → do
				(b,e) ← lineFold $ (,) <$> binding <*> (optionMaybe $ keyword "in" *> expression)
				case e of
					Nothing → do
						rec ← optionMaybe parseBindings
						case rec of
							Nothing → return ([b], Nothing)
							Just (bs, me) → return (b:bs, me)
					Just je → return ([b], Just je)
	(binds, e) ← keyword "let" *> block parseBindings
	case e of
		Nothing → L binds <$> (keyword "in" *> expression)
		Just je → return $ L binds je

binding ∷ IndentCharParser st (String,Λ)
binding = flip label "binding" $ do
	funct ← ident
	rhs ← flip (foldr Λ) <$> many ident <*> (sym "=" *> expression)
	return (funct, rhs)

keyword ∷ String → IndentCharParser st ()
keyword = reserved haskell

ident ∷ IndentCharParser st String
ident = identifier haskell

sym ∷ String → IndentCharParser st String
sym = symbol haskell
