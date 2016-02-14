
module Main where
import qualified Doc
import qualified Assertion
import qualified AnomDistr

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . AnomDistr.check . Assertion.smartDeviceParser)
	putStrLn ""

