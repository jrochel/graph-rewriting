{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module TermRewriting (buildRule) where

import Prelude.Unicode
import GraphRewriting
import Data.Char (isLower)
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Term
import Graph


type VarMap = Map Char Edge

buildRule ∷ (View Vertex n, View [Port] n) ⇒ Term → Term → Rule n
buildRule lhs rhs = do
	root ← edge
	varMap ← buildLHS root lhs
	buildRHS root varMap rhs

buildLHS ∷ (View Vertex n, View [Port] n) ⇒ Edge → Term → Pattern n VarMap
buildLHS root term = case term of
	Var v | isLower v → return $ Map.singleton v root
	Var v | otherwise → do
		c ← liftReader (edgeCardinality root)
		require (c ≡ 2)
		Variable {name = n} ← nodeWith root
		require (n ≡ v)
		return Map.empty
	App f x → do
		c ← liftReader (edgeCardinality root)
		require (c ≡ 2)
		Applicator {inp = i, out1 = o1, out2 = o2} ← nodeWith root
		require (i ≡ root)
		Map.unionWithKey nonLinear <$> buildLHS o1 f <*> buildLHS o2 x
		where nonLinear v = error $ "Left-hand side is not linear as " ⧺ [v] ⧺ " occurs twice"

buildRHS ∷ (View Vertex n, View [Port] n) ⇒ Edge → VarMap → Term → Rule n
buildRHS root bindings term = replace $ build term root where

	build (App f x) inc = do
		(xEdge,fEdge) ← (,) <$> byEdge <*> byEdge
		byNode Applicator {inp = inc, out1 = fEdge, out2 = xEdge}
		build f fEdge >> build x xEdge >> return ()

	build (Var v) inc = if isLower v
		then maybe freeVar (byWire inc) (Map.lookup v bindings)
		else byNode Variable {inp = inc, name = v}
		where freeVar = error $ v : " occurs free on the right-hand side"
