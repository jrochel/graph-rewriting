{-# LANGUAGE UnicodeSyntax #-}
module Term where

import Prelude.Unicode
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.IndentParser as Indent
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.IndentParser.Token
import Control.Monad (liftM)


data Λ = A Λ Λ            -- ^ application
       | Λ String Λ       -- ^ abstraction
       | V String         -- ^ variable
       | L [(String,Λ)] Λ -- ^ let binding
	deriving (Show,Eq,Ord)

parse ∷ String → Λ
parse str = either (error ∘ show) id (Indent.parse parser "(null)" str)

parseFile ∷ FilePath → IO Λ
parseFile = liftM (either (error ∘ show) id) ∘ Indent.parseFromFile parser

parser ∷ IndentCharParser st Λ
parser = expression where

	expression = flip label "expression" $ application <|> letBinding

	application = liftM (foldl1 A) $ many1 $ choice [parenthetic, abstraction, variable]

	parenthetic = parens haskell expression

	abstraction = flip label "abstraction" $ do
		_ ← sym "λ" <|> sym "\\"
		vars ← many1 ident
		_ ← sym "." <|> sym "→" <|> sym "->"
		body ← expression
		return $ foldr Λ body vars

	variable = liftM V $ ident <|> operator haskell <|> liftM (either show show) (naturalOrFloat haskell)

	-- Ugly, but works. Keyword "in" terminates binding blocks and bindings. Allows empty lets
	letBinding = flip label "let binding" $ do
		keyword "let"
		let parseBindings = do
			e ← optionMaybe $ keyword "in" >> expression
			case e of
				Just je → return ([], Just je)
				Nothing → do
					(b,e) ← lineFold $ do
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
		(binds, e) ← block parseBindings
		case e of
			Nothing → liftM (L binds) (keyword "in" >> expression)
			Just je → return $ L binds je

	binding ∷ IndentCharParser st (String,Λ)
	binding = flip label "binding" $ do
		funct  ← ident
		params ← many ident
		body   ← sym "=" >> expression
		return (funct, foldr Λ body params)

	keyword = reserved haskell
	ident = identifier haskell
	sym = symbol haskell
