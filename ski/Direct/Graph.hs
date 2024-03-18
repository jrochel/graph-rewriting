{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances #-}
module Direct.Graph where

import Data.View
import GraphRewriting.Graph
import GraphRewriting.Graph.Write
import qualified Common.Term as Term


-- <chunk: node_type>
data SKI
	=  S           {inp ∷ Port}
	|  K           {inp ∷ Port}
	|  I           {inp ∷ Port}
	|  Applicator  {inp, out1, out2 ∷ Port}
	|  Duplicator  {inp1, inp2, out ∷ Port}
	|  Eraser      {inp ∷ Port}
	|  Variable    {inp ∷ Port, name ∷ String}
	|  Root        {out ∷ Port}
-- </chunk: node_type>

-- <chunk: view_ports>
instance View [Port] SKI where
	inspect ski = case ski of
		S           {inp = i}                        → [i]
		K           {inp = i}                        → [i]
		I           {inp = i}                        → [i]
		Applicator  {inp = i, out1 = o1, out2 = o2}  → [i,o1,o2]
		Duplicator  {inp1 = i1, inp2 = i2, out = o}  → [i1,i2,o]
		Eraser      {inp = i}                        → [i]
		Variable    {inp = i}                        → [i]
		Root        {out = o}                        → [o]
	update ports ski = case ski of
		S           {} → ski {inp = i}                        where [i]        = ports
		K           {} → ski {inp = i}                        where [i]        = ports
		I           {} → ski {inp = i}                        where [i]        = ports
		Applicator  {} → ski {inp = i, out1 = o1, out2 = o2}  where [i,o1,o2]  = ports
		Duplicator  {} → ski {inp1 = i1, inp2 = i2, out = o}  where [i1,i2,o]  = ports
		Eraser      {} → ski {inp = i}                        where [i]        = ports
		Variable    {} → ski {inp = i}                        where [i]        = ports
		Root        {} → ski {out = o}                        where [o]        = ports
-- </chunk: view_ports>

fromTerm ∷ Term.Expr → Graph SKI
fromTerm term = flip execGraph emptyGraph $ do
	e ← compile term
	newNode Root {out = e}

compile ∷ Term.Expr → Rewrite SKI Edge
compile term = do
	e ← newEdge
	_ ← case term of
		Term.A f x → do
			ef ← compile f
			ex ← compile x
			newNode Applicator {inp = e, out1 = ef, out2 = ex}
		Term.S → newNode S {inp = e}
		Term.K → newNode K {inp = e}
		Term.I → newNode I {inp = e}
		Term.V v → newNode Variable {inp = e, name = v}
	return e
