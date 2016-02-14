
module NetDot where
import qualified FwDot
import Net

getDotFromDev :: Device -> String
getDotFromDev (Router _ _) = ""
getDotFromDev (Filter name _ rules) = concat [
	"\tsubgraph cluster", name, " {\n",
	"\tlabel=\"", name, "\"\n",
	FwDot.getRuleTreeDotContentsForFw name rules,
	"\t}\n"
	]

getDotFromDevs :: [Device] -> String
getDotFromDevs devs = concat [
	"digraph {\n",
	concatMap getDotFromDev devs,
	"}\n"
	]

getEquivsDotFromDev :: Device -> String
getEquivsDotFromDev (Router _ _) = ""
getEquivsDotFromDev (Filter name _ rules) = concat [
	"\tsubgraph cluster", name, " {\n",
	"\t\tlabel=\"", name, "\"\n",
	FwDot.getEquivsDotContentsForFw name rules,
	"\t}\n"
	]

getEquivsDotFromDevs :: [Device] -> String
getEquivsDotFromDevs devs = concat [
	"digraph  {\nranksep=0\nedge [ style=invis minlen=1 ]\nnode [ shape=plaintext ]",
	concatMap getEquivsDotFromDev devs,
	"}\n"
	]

