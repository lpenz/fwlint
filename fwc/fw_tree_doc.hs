
module Main where
import qualified Doc
import qualified Parse
import Rule

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . buildTree . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

