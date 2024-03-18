{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Main where

import Prelude.Unicode
import Control.Monad
import GraphRewriting.GL.Render
import GraphRewriting.GL.UI as UI
import Term
import Graph
import GL ()
import Rules
import TermRewriting
import GraphRewriting
import GraphRewriting.Graph.Write.Unsafe as Unsafe
import GraphRewriting.Layout.Coulomb
import GraphRewriting.Layout.SpringEmbedder
import GraphRewriting.Layout.Gravitation
import GraphRewriting.Layout.Wrapper
import System.FilePath.Posix
import System.Directory
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (catMaybes)


instance Render (Wrapper Vertex) where render = render . wrappee

parseFiles ∷ (View Vertex n, View [Port] n) ⇒ Set FilePath → Set FilePath → IO [LabelledTree (Rule n)]
parseFiles done todo = if Set.null todo
	then return [Branch "Beaurocratic" [Leaf "Erase" eraseRule, Leaf "Unshare" unshare]]
	else let f = Set.findMin todo in do
		(imports, branch) ← parseFile f
		let done' = Set.insert f done
		let todo' = Set.union todo imports `Set.difference` done'
		fmap (branch:) (parseFiles done' todo')

parseFile ∷ (View Vertex n, View [Port] n) ⇒ FilePath → IO (Set FilePath, LabelledTree (Rule n))
parseFile f = do
	((imports,rules),parseErrors) ← fmap (parse ruleset) (readFile f)
	unless (null parseErrors) $ do
		putStrLn $ "Parse errors in " ⧺ f ⧺ ":"
		putStr $ unlines $ map show parseErrors
	imports ← fmap (Set.fromList . catMaybes) (mapM checkImport imports)
	return (imports, Branch (takeBaseName f) [Leaf (show l ⧺ " -> " ⧺ show r) (buildRule l r) | (l,r) ← rules])
	where checkImport i = do
		i ← canonicalizePath $ takeDirectory f `combine` i
		exists ← doesFileExist i
		if not exists
			then do
				putStrLn $ "Import error in " ⧺ f ⧺ ": " ⧺ i ⧺ " missing"
				return Nothing
			else return $ Just i

main ∷ IO ()
main = do
	(prog,args) ← UI.initialise
	let (files,termInput) = if length args ≥ 2
		then (init args, last args)
		else error $ "usage: " ⧺ prog ⧺ " [GLUT-options] <rules.trs> ... <term>"

	trsRules ← parseFiles Set.empty . Set.fromList =<< mapM canonicalizePath files

	let (t,parseErrors) = parse term termInput
	unless (null parseErrors) $ do
		putStrLn "Parse errors in input term:"
		putStr $ unlines $ map show parseErrors

	UI.run 40 id layoutStep (wrapGraph $ fromTerm t) (Branch "All" trsRules)

layoutStep n = do
	(cgf, cf, sf, rot) ← readOnly $ do
		cgf ← centralGravitation n
		cf ← coulombForce n
		sf ← springForce 1.5 n
		rot ← angularMomentum n
		return (cgf, cf, sf, rot)
	Unsafe.modifyNode n $ adjust $ Position . sf (*0.9) . cgf (*0.01) . cf (\x → min (100/(x^2+0.1)) 10) . position
