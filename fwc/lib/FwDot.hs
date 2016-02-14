
module FwDot(getRuleTreeDotContentsForFw, getRuleTreeDotForFw, getEquivsDotContentsForFw, getEquivsDotForFw) where

import qualified Doc
import Rule
import qualified Action
import qualified Range

debug :: Bool
debug = False

rulename :: Rule -> String
rulename = (drop 5) . Doc.showname

nodename :: String -> (Int, Bool, Range.Tree Rule) -> String
nodename sid (n, b, (Range.Inner r _ _)) = concat [ "a", sid, Doc.showname r, if b then "ins" else "out", show n ]
nodename sid (n, _, (Range.Leaf m)) = concat [ "a", sid, getDotLabelForLeaf m, show n ]

getName :: String -> [(Int, Bool, Range.Tree Rule)] -> String
getName sid me = concatMap (nodename sid) me

getLabel :: Range.Tree Rule -> (Int, Bool, Range.Tree Rule) -> String
getLabel _ (_, _, (Range.Inner rule _ _)) = (if not debug then rulename else Doc.showrepr) rule
getLabel _ (_, _, (Range.Leaf [])) = ""
getLabel (Range.Inner rule _ _) (_, _, (Range.Leaf ms)) = if not debug then "" else (Doc.showrepr rule ++ "\\n" ++ getDotLabelForLeaf ms)
getLabel _ _ = undefined

label :: Bool -> String
label b = if b then "in" else "out"

getLabelEdge :: (Int, Bool, Range.Tree Rule) -> String
getLabelEdge (_, b, _) = label b

getColorEdge :: (Int, Bool, Range.Tree Rule) -> String
getColorEdge (_, True, _) = "blue"
getColorEdge (_, False, _) = "orange"

getDotLabelForLeaf :: [Rule] -> String
getDotLabelForLeaf (m:ms) = Doc.showrepr m ++ "\\n" ++ getDotLabelForLeaf ms
getDotLabelForLeaf _ = ""

getDotForLeaf :: String -> [(Int, Bool, Range.Tree Rule)] -> (Int, Bool, Range.Tree Rule) -> Range.Tree Rule -> String
getDotForLeaf sid ancestry me@(_, _, (Range.Leaf _)) lastin = concat [
		"\t\t\"", getName sid ancestry, "\" -> \"", getName sid (ancestry++[me]), "\" [ label=\"", getLabelEdge me, "\" color=", getColorEdge me, " ]\n",
		"\t\t\"", getName sid (ancestry++[me]), "\" [ shape=\"", shape, "\" label=\"", getLabel lastin me, "\" style=filled fillcolor=", cor, " ]\n" ]
		where
			shape = if not debug then "circle" else "box"
			cor = case (me, lastin) of
				((_, _, Range.Leaf []), _) -> "black"
				(_, (Range.Inner (Rule _ _ Action.Accept) _ _)) -> "green"
				(_, (Range.Inner (Rule _ _ Action.Deny) _ _)) -> "red"
				_ -> undefined
getDotForLeaf _ _ _ _ = error "getDotForLeaf"

getDotForChild :: String -> [(Int, Bool, Range.Tree Rule)] -> (Int, Bool, Range.Tree Rule) -> (Int, Bool, Range.Tree Rule) -> Range.Tree Rule -> String
getDotForChild sid ancestry (_, _, Range.Inner _ _ _) me@(_, _, Range.Inner _ _ _) _ = concat [ "\t\t\"", getName sid ancestry, "\" -> \"", getName sid (ancestry++[me]), "\" [ label=\"", getLabelEdge me, "\" color=", getColorEdge me, " ]\n" ]
getDotForChild sid ancestry (_, _, _) me@(_, _, _) lastin = getDotForLeaf sid ancestry me lastin

getDotFromNode :: String -> [(Int, Bool, Range.Tree Rule)] -> (Int, Bool, Range.Tree Rule) -> Range.Tree Rule -> String
getDotFromNode _ _ (_, _, Range.Leaf _) _ = ""
getDotFromNode sid ancestry me@(n, _, mynode@(Range.Inner _ i o)) lastin = concat [
	"\t\t\"", getName sid ame, "\" [ label=\"", getLabel lastin me, "\" ]\n",
	getDotForChild sid ame me (n+1, True, i) mynode,
	getDotForChild sid ame me (n+1, False, o) lastin,
	getDotFromNode sid ame (n+1, True, i) mynode,
	getDotFromNode sid ame (n+1, False, o) lastin
	]
	where ame = ancestry ++ [me]

getDotFromMatchActionTree :: String -> Range.Tree Rule -> String
getDotFromMatchActionTree sid mat = concat [
	"digraph {\n",
	getDotFromNode sid [] (0, False, mat) mat,
	"}\n"
	]

getRuleTreeDotContentsForFw :: String -> [Rule] -> String
getRuleTreeDotContentsForFw sid rules = (getDotFromNode sid [] (0, False, tree) tree)
	where
		tree = buildTree rules

getRuleTreeDotForFw :: [Rule] -> String
getRuleTreeDotForFw = (getDotFromMatchActionTree "") . buildTree

------------------------------------------------------------------------------

getEquivDot :: String -> (Int, [Rule]) -> String
getEquivDot sid (i, rules) = concat [
	"\t\tsubgraph cluster", sid, show i, " {\n",
	--"\t\t\tlabel=", show i, "\n",
	"\t\t\tlabel=\"\"\n",
	concatMap ( \ r -> concat [ "\t\t\te", sid, show i, drop 5 $ Doc.showname r, " [ label=\"", drop 5 (Doc.showname r), "\" style=filled fillcolor=", if getAction r == Action.Accept then "green" else "red", " ]\n"]) rules,
	"\t\t\t", connect $ map ( \ r -> concat [ "e", sid, show i, rulename r]) rules,
	"\n\t\t}\n"
	]
	where
		connect :: [String] -> String
		connect (r:[]) = r
		connect (r:rs) = r ++ " -> " ++ connect rs
		connect _ = ""

--getEquivDot (i, rules) = concat [
--	"eq", show i, " [ shape=record label=\"{<b>", show i, "</b>|",
--	concatMap ( \ r -> concat [ drop 5 (Doc.showname r), "\\n"]) rules,
--	"}\" ]\n"
--	]

getEquivsDotContentsForFw :: String -> [Rule] -> String
getEquivsDotContentsForFw sid rules = concatMap (getEquivDot sid) (zip [1..] (Range.buildEquivalent $ buildTree rules))

getEquivsDotForFw :: [Rule] -> String
getEquivsDotForFw rules = concat [
	"digraph  {\nranksep=0\nedge [ style=invis minlen=1 ]\nnode [ shape=plaintext ]",
	getEquivsDotContentsForFw "" rules,
	"}\n"
	]

