{-# LANGUAGE UnicodeSyntax #-}
-- | Term to graph transformation where variable names are resolved to graph edges (or constants)
module Resolver (resolve) where

import Prelude.Unicode
import Term
import Graph
import GraphRewriting.Graph
import GraphRewriting.Graph.Write
import Control.Monad (liftM, zipWithM)


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
		newNode Applicator {inp = p, func = f, arg = x}
		compile env f func
		compile env x arg
	Λ x e → do
		b ← newEdge
		(v, name) ← bindName True x
		newNode Abstractor {inp = p, body = b, var = v}
		compile (name : env) b e
	V var → case env of
		[  ] → newNode Constant {inp = p, name = var} >> return ()
		n:ns → if var ≡ symbol n
			then mergeEdges p =<< reference n
			else if boundByLambda n
				then do
					p' ← newEdge
					newNode Delimiter {level = 0, inp = p', out = p}
					compile ns p' term
				else compile ns p term
	L binds e → do
		(es, names) ← liftM unzip $ mapM (bindName False) (map fst binds)
		let env' = names ⧺ env
		zipWithM (compile env') es (Prelude.map snd binds)
		compile env' p e

bindName ∷ Bool → String → Compiler (Edge, Name)
bindName lambda sym = do
	v ← newEdge
	s ← newNode Multiplier {out = v, ins = []}
	let ref = do
		e ← newEdge
		modifyNode s $ \s → s {ins = e : ins s}
		return e
	return (v, Name {symbol = sym, boundByLambda = lambda, reference = ref})
