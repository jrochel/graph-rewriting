{-# LANGUAGE UnicodeSyntax #-}
-- | Term to graph transformation where variable names are resolved to graph edges (or constants)
module Resolver (resolve) where

import Prelude.Unicode
import Term
import Graph
import GraphRewriting.Graph
import GraphRewriting.Graph.Write
import Control.Monad (liftM, zipWithM)


type Compiler = Rewrite NodeWW

type Environment = [Name]
data Name = Name {symbol ∷ String, reference ∷ Compiler Edge}

resolve ∷ Λ → Graph NodeWW
resolve term = flip execGraph emptyGraph $ do
	o ← newEdge
	i ← newNode Initiator {out = o}
	compile [] o term

compile ∷ Environment → Edge → Λ → Compiler ()
compile env p term = case term of
	A func arg → do
		f ← newEdge
		x ← newEdge
		_ ← newNode Applicator {inp = p, func = f, arg = x}
		compile env f func
		compile env x arg
	Λ x e → do
		b ← newEdge
		(v, n) ← bindName x
		_ ← newNode Abstractor {name = x, inp = p, body = b, var = v}
		compile (n : env) b e
	V var → case env of
		[  ] → newNode Primitive {inp = p, name = var} >> return ()
		n:ns → if var ≡ symbol n
			then mergeEdges p =<< reference n
			else compile ns p term
	L binds e → do
		(es, names) ← liftM unzip $ mapM bindName (map fst binds)
		let env' = names ⧺ env
		_ ← zipWithM (compile env') es (Prelude.map snd binds)
		compile env' p e

bindName ∷ String → Compiler (Edge, Name)
bindName sym = do
	v ← newEdge
	s ← newNode Multiplexer {out = v, ins = []}
	let ref = do
		e ← newEdge
		modifyNode s $ \s → s {ins = e : ins s}
		return e
	return (v, Name {symbol = sym, reference = ref})
