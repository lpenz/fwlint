-- | Simple graphviz output.

#ifdef PROFILE

module Graphviz where

#else

module Graphviz(
		dot,
		neato,
		buildNeatoFromMap
) where

import Data.Graph.Inductive.Graph
import qualified Map
import qualified Doc

-- | Formats a graph for use in graphviz.
graphviz :: (Graph g, Show a, Show b) =>
	String -- ^ Graph init
	-> String -- ^ Separator string
	-> String -- ^ Default graph properties
	-> g a b -- ^ The graph to format
	-> String
graphviz i s p g =
	let
		n = labNodes g
		e = labEdges g
		ns = concatMap sn n
		es = concatMap se e
	in concat [ i, " {\n"
		, p
		, ns
		, es
	, "}" ]
		where
			sn (n, a)
				| sa == ""	= ""
				| otherwise	= concat [ "\t", show n, sa, "\n" ]
				where
					sa = sl a
			se (n1, n2, b) = concat [ "\t", show n1, " ", s, " ", show n2, sl b, "\n" ]

dot :: (Graph g, Show a, Show b) => g a b -> String
dot = graphviz "digraph" "->" "\tnslimit=\"50\"\n"

neato :: (Graph g, Show a, Show b) => g a b -> String
neato = graphviz "graph" "--" "\tnslimit=\"50\"\n\toverlap=\"false\"\n"

buildNeatoFromMap :: (Doc.Able a, Doc.Able b, Doc.Able c) => Map.Map (a, b) c -> String
buildNeatoFromMap m = concat [
	"graph {\n",
	"\toverlap=\"false\"\n",
	"\tnslimit=\"50\"\n",
	edgestr,
	"}\n" ]
	where
		printEdge :: (Doc.Able a, Doc.Able b, Doc.Able c) => ((a, b), c) -> String
		printEdge ((n1, n2), c) = concat [ "\t\"", (Doc.showrepr) n1, "\" -- \"", (Doc.showrepr) n2, "\" [ label=\"", (Doc.showrepr) c, "\" ]\n" ]
		edgestr = concatMap printEdge $ Map.toList $ m

sq :: String -> String
sq ('"':s)
	| last s == '"' = init s
	| otherwise	= s
sq ('\'':s)
	| last s == '\'' = init s
	| otherwise	= s
sq s = s

sl :: (Show a) => a -> String
sl a =
	let l = sq (show a)
	in if (l /= "()") then (" [label = \"" ++ l ++ "\"]") else ""

#endif

