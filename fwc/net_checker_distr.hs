
module Main where
import qualified Net
import qualified Checker
import qualified Parse
import qualified Assertion

main :: IO ()
main = do
	interact (Checker.show . (filter Checker.isDistr) . Checker.checkEither . (Parse.runParser :: String -> Either [Net.Device] ([Net.Device], [Assertion.Assertion])))
	putStrLn ""

