
module Main where
import qualified Doc
import qualified Parse
import qualified AnomIsol
import Rule

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . AnomIsol.check . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

