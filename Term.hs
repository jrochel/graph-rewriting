{-# LANGUAGE UnicodeSyntax #-}
module Term where

import Prelude.Unicode
import Text.Parsec
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import Text.Parsec.IndentParsec
import Control.Monad.Identity


data Λ = A Λ Λ            -- ^ application
       | Λ String Λ       -- ^ abstraction
       | V String         -- ^ variable
       | L [(String,Λ)] Λ -- ^ let binding
	deriving (Show,Eq,Ord)

parseFile ∷ FilePath → IO Λ
parseFile f = do
	file ← readFile f
	let parseErrorOrExpr = runIdentity $ runGIPT parser () f file
	return $ either (error ∘ show) id parseErrorOrExpr

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

tokP :: IndentTokenParser String () Identity
tokP = makeTokenParser langDef

parser ∷ IndentCharParser Λ
parser = expression where

	expression = flip label "expression" $ application <|> letBinding

	application = liftM (foldl1 A) $ many1 $ choice [parenthetic, abstraction, variable]

	parenthetic = parens tokP expression

	abstraction = flip label "abstraction" $ do
		_ ← sym "λ" <|> sym "\\"
		vars ← many1 ident
		_ ← sym "." <|> sym "→" <|> sym "->"
		body ← expression
		return $ foldr Λ body vars

	variable = liftM V $ ident <|> operator tokP <|> liftM (either show show) (naturalOrFloat tokP)

	-- Ugly, but works. Keyword "in" terminates binding blocks and bindings. Allows empty lets
	letBinding = flip label "let binding" $ do
		keyword "let"
		let parseBindings = do
			e ← optionMaybe $ keyword "in" >> expression
			case e of
				Just je → return ([], Just je)
				Nothing → do
					(b,e) ← foldedLinesOf $ do
						b ← binding
						e ← optionMaybe $ keyword "in" >> expression
						return (b,e)
					case e of
						Nothing → do
							rec ← optionMaybe parseBindings
							case rec of
								Nothing → return ([b], Nothing)
								Just (bs, me) → return (b:bs, me)
						Just je → return ([b], Just je)
		(binds, e) ← blockOf parseBindings
		case e of
			Nothing → liftM (L binds) (keyword "in" >> expression)
			Just je → return $ L binds je

	binding ∷ IndentCharParser (String,Λ)
	binding = flip label "binding" $ do
		funct  ← ident
		params ← many ident
		body   ← sym "=" >> expression
		return (funct, foldr Λ body params)

	keyword = reserved tokP
	ident = identifier tokP
	sym = symbol tokP
