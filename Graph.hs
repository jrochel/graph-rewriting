{-# LANGUAGE UnicodeSyntax, FlexibleInstances, MultiParamTypeClasses #-}
module Graph where

import Prelude.Unicode
import Data.View
import GraphRewriting.Graph.Types
import GraphRewriting.Pattern.InteractionNet


data NodeLS
	= Initiator  {out ∷ Port}
	| Applicator {inp, func, arg ∷ Port}
	| Abstractor {inp, body, var ∷ Port}
	| Constant   {inp ∷ Port, name ∷ String}
	| Function   {inp ∷ Port, out ∷ Port , name ∷ String}
	| Eraser     {inp ∷ Port}
	| Duplicator {level ∷ Int, inp, out1, out2 ∷ Port}
	| Delimiter  {level ∷ Int, inp, out ∷ Port}
	| Multiplier {out ∷ Port, ins ∷ [Port]} -- only intermediate compilation result

-- | equality as defined in the paper with only the relevant cases included
instance Eq NodeLS where
	Eraser     {}           == Eraser     {} = True
	Duplicator {level = l1} == Duplicator {level = l2} = l1 ≡ l2
	Delimiter  {level = l1} == Delimiter  {level = l2} = l1 ≡ l2
	_ == _ = False

instance View [Port] NodeLS where
	inspect node = case node of
		Initiator  {out = o}                       → [o]
		Applicator {inp = i, func = f, arg = a}    → [i,f,a]
		Abstractor {inp = i, body = b, var = v}    → [i,b,v]
		Constant   {inp = i}                       → [i]
		Function   {inp = i, out = o}              → [i,o]
		Eraser     {inp = i}                       → [i]
		Duplicator {inp = i, out1 = o1, out2 = o2} → [i,o1,o2]
		Delimiter  {inp = i, out = o}              → [i,o]
		Multiplier {out = o, ins = is}             → o:is
	update ports node = case node of
		Initiator  {} → node {out = o}                       where [o]       = ports
		Applicator {} → node {inp = i, func = f, arg = a}    where [i,f,a]   = ports
		Abstractor {} → node {inp = i, body = b, var = v}    where [i,b,v]   = ports
		Constant   {} → node {inp = i}                       where [i]       = ports
		Function   {} → node {inp = i, out = o}              where [i,o]     = ports
		Eraser     {} → node {inp = i}                       where [i]       = ports
		Duplicator {} → node {inp = i, out1 = o1, out2 = o2} where [i,o1,o2] = ports
		Delimiter  {} → node {inp = i, out = o}              where [i,o]     = ports
		Multiplier {} → node {out = o, ins = is}             where o:is      = ports

instance INet NodeLS where principalPort = pp 
	
pp ∷ NodeLS → Int 
pp node = case node of
	Initiator  {} → 0
	Applicator {} → 1
	Abstractor {} → 0
	Delimiter  {} → 0
	Constant   {} → 0
	Function   {} → 1
	Duplicator {} → 0
	Eraser     {} → 0
