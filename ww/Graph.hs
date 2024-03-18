{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses #-}
module Graph where

import Data.View
import GraphRewriting.Graph.Types


data NodeWW
	= Initiator   {out ∷ Port}
	| Applicator  {inp, func, arg ∷ Port}
	| Abstractor  {name ∷ String, inp, body, var ∷ Port}
	| Primitive   {inp ∷ Port, name ∷ String}
	| Eraser      {inp ∷ Port}
	| Duplicator  {active ∷ Bool, inp, out1, out2 ∷ Port}
	| Multiplexer {out ∷ Port, ins ∷ [Port]} -- only intermediate compilation result

instance View [Port] NodeWW where
	inspect node = case node of
		Initiator   {out = o}                       → [o]
		Applicator  {inp = i, func = f, arg = a}    → [i,f,a]
		Abstractor  {inp = i, body = b, var = v}    → [i,b,v]
		Primitive   {inp = i}                       → [i]
		Eraser      {inp = i}                       → [i]
		Duplicator  {inp = i, out1 = o1, out2 = o2} → [i,o1,o2]
		Multiplexer {out = o, ins = is}             → o:is
	update ports node = case node of
		Initiator   {} → node {out = o}                       where [o]       = ports
		Applicator  {} → node {inp = i, func = f, arg = a}    where [i,f,a]   = ports
		Abstractor  {} → node {inp = i, body = b, var = v}    where [i,b,v]   = ports
		Primitive   {} → node {inp = i}                       where [i]       = ports
		Eraser      {} → node {inp = i}                       where [i]       = ports
		Duplicator  {} → node {inp = i, out1 = o1, out2 = o2} where [i,o1,o2] = ports
		Multiplexer {} → node {out = o, ins = is}             where o:is      = ports
