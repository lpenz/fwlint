
module Main where
import qualified Doc
import qualified Parse
import qualified Range
import Rule

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . (map snd) . Range.buildDisjunct . rules)
	putStrLn ""
	where
		rules :: String -> [Rule]
		rules = Parse.runParser

