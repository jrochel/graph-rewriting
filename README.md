# Port graph rewriting in Haskell

These Haskell packages facilitate the interactive exploration of rewriting systems.

Some of packages supply applications in order to experiment with pre-defined
rewriting systems (graph-rewriting-ski, graph-rewriting-lambdascope), other
applications allow for the user to define their own rewriting system
(graph-rewriting-trs).

Other packages contain libraries allowing for the development of more rewriting
systems (graph-rewriting, graph-rewriting-layout, graph-rewriting-gl)

The formalism underlying this project is graph rewriting and on top of that
port graph rewriting. One can then express term rewriting rules using the port
graph rewriting formalism.

## Tutorial

The graph-rewriting-ski package sources are commented in tutorial style.

## Packages

- graph-rewriting: base package defining an EDSL for specifying monadic graph rewriting rules
- graph-rewriting-layout: force-directed layouting useful for graphical rendering of port graphs
- graph-rewriting-gl: OpenGL interface by which one can build interactive applications to interactively perform graph reductions
- graph-rewriting-ski: implementation of the SKI combinators, which also serves as a tutorial to the package suite.
- graph-rewriting-trs: Evaluate a first-order term rewrite system interactively using graph reduction. You don't need to know Haskell to use this tool. Just define your rewrite rules in a separate file and give an initial term. This package could in principle replace graph-rewriting-ski, but is far more generic and involved and therefore not suited for a tutorial.
- graph-rewriting-lambdascope: An implementation of an optimal evaluator for the λ-calculus, Lambdascope
- maxsharing: implementation of maximal sharing from the corresponding paper above.
