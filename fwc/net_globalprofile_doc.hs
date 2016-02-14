
module Main where
import qualified Doc
import qualified Assertion
import qualified Net
import qualified AnomDistr
import qualified Tuple

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . Tuple.e1of3 . buildGlobalProfileAnomDistr_from_devs . Assertion.smartDeviceParser)
	putStrLn ""
	where
		buildGlobalProfileAnomDistr_from_devs devs = (AnomDistr.buildGlobalProfileAnomDistr devs) $ Net.allPaths devs

