
module Main where
import FwDot
import qualified Parse
import Rule

main :: IO ()
main = do
	interact (getRuleTreeDotForFw . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

