
module Main where
import qualified Doc
import qualified Parse
import qualified Maybe
import Action
import Match
import Rule

texMatch :: Match -> String
texMatch m = Doc.render $ Doc.hcat $ Maybe.catMaybes [
		docSocket "" (maAsrc m, maPsrc m),
		Just $ Doc.repr " & ",
		docSocket "" (maAdst m, maPdst m)
	]

texRule :: Rule -> String
texRule (Rule name match Accept) = concat [ name, " & ", texMatch match, " & ", "{\\color{green}Accept}\\\\\n" ]
texRule (Rule name match Deny)   = concat [ name, " & ", texMatch match, " & ", "{\\color{red}  Deny}\\\\\n" ]
texRule _ = ""

texRules :: [Rule] -> String
texRules rules = concat [
	"\\begin{tabular}{lllc}\n",
	"Name & Source & Destination & Target \\\\\n",
	"\\hline\n",
	concatMap texRule rules,
	"\\end{tabular}\n"
	]

main :: IO ()
main = do
	interact (texRules . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

