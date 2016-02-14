
module Main where
import qualified Assertion
import qualified Net
import qualified Rule
import qualified Match
import qualified Action
import qualified Ip
import qualified Maybe
import qualified Map
import qualified Range
import qualified Doc

maSocketString :: Map.Map (Ip.Addr, Int) String -> (Range.Range Ip.Addr, Range.Range Ip.Port) -> String
maSocketString mp (a, p) = concat [
	if a /= Match.ipRangeTotal
		then ipstr
		else "any",
	portstr
	]
	where
		ipstr = case Ip.getRangeIpNet a of
			Nothing -> defstr
			Just ipnet -> if Map.member ipnet mp && (not $ null $ mp Map.! ipnet) then (mpstr ipnet) else defstr
		defstr = Doc.showrepr a
		mpstr ipnet = "{\\tt{" ++ ((Map.!) mp ipnet) ++ "}}"
		portstr = if p /= Match.portRangeTotal then ':':(Doc.showrepr p) else ""

maybeTex :: String -> Maybe String -> String
maybeTex str mb = concat [ str, " ", maybe "any" id mb ]

texAction :: Action.Action -> String
texAction action@Action.Accept = "{\\color{green} " ++ show action ++ "}"
texAction action@Action.Deny   = "{\\color{red} "   ++ show action ++ "}"

texRule :: Map.Map (Ip.Addr, Int) String -> Rule.Rule -> String
texRule mp (Rule.Rule name match action) = concat [
	"{\\tt{", name, "}} & ",
	"from ", (maSocketString mp $ Match.maGetSrc match),
	" & ",
	"to ",   (maSocketString mp $ Match.maGetDst match),
	" & $\\rightarrow$ & ",
	texAction action,
	"\\\\\n"
	]
texRule _ _ = ""

texAssertion :: Map.Map (Ip.Addr, Int) String -> Assertion.Assertion -> String
texAssertion mp (Assertion.Assertion name match action) = concat [
	"{\\tt{", name, "}} & ",
	"from ", (maSocketString mp $ Match.maGetSrc match),
	" & ",
	"to ",   (maSocketString mp $ Match.maGetDst match),
	" & $\\rightarrow$ & ",
	texAction action,
	"\\\\\n"
	]

texAssertions :: Map.Map (Ip.Addr, Int) String -> [Assertion.Assertion] -> String
texAssertions mp assertions = concat [
	"\\item Assertions:\n",
	"\\begin{tabular}{|lllcl}",
	concatMap (texAssertion mp) assertions,
	"\\end{tabular}\n"
	]

texDevsAssertions :: ([Net.Device], [Assertion.Assertion]) -> String
texDevsAssertions (devs, assertions) = concat [
	"\\begin{itemize}\n",
	texAssertions mp assertions,
	"\\end{itemize}\n"
	]
	where mp = Net.getNetworkNameMap devs

main :: IO ()
main = do
	interact (texDevsAssertions . Assertion.smartDeviceAssertionParser)
	putStrLn ""

