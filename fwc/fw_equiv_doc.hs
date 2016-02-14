
module Main where
import qualified Doc
import qualified Parse
import qualified Range
import Rule

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . Range.buildEquivalentWithRegion . buildTree . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

