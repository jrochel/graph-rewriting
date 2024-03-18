{-# LANGUAGE UnicodeSyntax #-}
-- | Term to graph transformation where variable names are resolved to graph edges (or constants)
module Resolver (resolve) where

import Prelude.Unicode
import Term
import Graph
import GraphRewriting.Graph
import GraphRewriting.Graph.Write
import Control.Monad
import Data.Maybe (fromMaybe)


type Compiler = Rewrite NodeLS

type Environment = [Name]
data Name = Name {symbol ∷ String, boundByLambda ∷ Bool, reference ∷ Compiler Edge}

resolve ∷ Λ → Graph NodeLS
resolve term = flip execGraph emptyGraph $ do
	o ← newEdge
	i ← newNode Initiator {out = o}
	compile [] o term

compile ∷ Environment → Edge → Λ → Compiler ()
compile env p term = case term of
	A func arg → do
		f ← newEdge
		x ← newEdge
		void $ newNode Applicator {inp = p, func = f, arg = x}
		compile env f func
		compile env x arg
	Λ x e → do
		b ← newEdge
		(v, name) ← bindName True x
		void $ newNode Abstractor {inp = p, body = b, var = v, name = x}
		compile (name : env) b e
	L binds e → do
		(es, names) ← liftM unzip $ mapM (bindName False) (map fst binds)
		let env' = names ⧺ env
		void $ zipWithM (compile env') es (Prelude.map snd binds)
		compile env' p e
	C name [] → case env of
		[  ] → void $ newNode $ fromMaybe (Constant {inp = p, name = name, args = []}) (operator name p)
		n:ns → if name ≡ symbol n
			then mergeEdges p =<< reference n
			else if boundByLambda n
					then do
						p' ← newEdge
						void $ newNode Delimiter {level = 0, inp = p', out = p}
						compile ns p' term
					else compile ns p term
	Term.Case exp cases → do
		let (pats, _) = unzip cases
		alts ← replicateM (length cases) newEdge -- edges going from the Case node to the CaseAlts
		o    ← newEdge
		void $ newNode Graph.Case {inp = p, out = o, alts = alts, names = map constr pats}
		mapM_ (\(alt, (pat,exp)) → compile env alt (foldr Λ exp (vars pat))) (zip alts cases) -- compile the different cases
		compile env o exp -- compile the scrutiny

operator ∷ String → Edge → Maybe NodeLS
operator n p = case n of
	"+" → op 2 $ liftM (show . sum) . mapM read
	"-" → op 2 $ liftM (show . minus) . mapM read where minus [x,y] = x - y
	"==" → op 2 $ \[x,y] → Just $ if x ≡ y then "T" else "F"
	_ → Nothing
	where

	read ∷ Read a ⇒ String → Maybe a
	read str = case [ x | (x, "") ← reads str ] of
		[] → Nothing
		x:_ → Just x

	op a f = Just Operator
		{inp = p, ops = [], arity = a, lmop = 0, function = f, name = n}

bindName ∷ Bool → String → Compiler (Edge, Name)
bindName lambda sym = do
	v ← newEdge
	s ← newNode Multiplexer {out = v, ins = []}
	let ref = do
		e ← newEdge
		modifyNode s $ \s → s {ins = e : ins s}
		return e
	return (v, Name {symbol = sym, boundByLambda = lambda, reference = ref})
