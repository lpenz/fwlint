
module Main where
import qualified Doc
import qualified Assertion
import qualified Net
import qualified System
import qualified Rule

main :: IO ()
main = do
	args <- System.getArgs
	case args of
		[] -> error "Filter name not supplied"
		[a] -> showfilter a
		_ -> error "Only one filter must be supplied"
	putStrLn ""
	where
		devs :: String -> [Net.Device]
		devs = Assertion.smartDeviceParser
		showfilter :: String -> IO ()
		showfilter f = interact (Doc.render . Doc.repr . (getRulesOfFilter f) . devs)

getRulesOfFilter :: String -> [Net.Device] -> [Rule.Rule]
getRulesOfFilter f ds = case (filter (filterer f) ds) of
	[] -> error ("Filter "++f++" not found")
	[(Net.Filter _ _ rules)] -> rules
	_ -> undefined

filterer :: String -> Net.Device -> Bool
filterer s (Net.Filter f _ _) = s == f
filterer _ _ = False

