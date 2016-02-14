
module Main where
import qualified Net
import qualified Checker
import qualified Parse
import qualified Assertion

isolfilt :: Checker.Error -> Bool
isolfilt (Checker.ErrorAnomIsol _) = True
isolfilt _ = False

main :: IO ()
main = do
	interact (Checker.show . (filter isolfilt) . Checker.checkEither . (Parse.runParser :: String -> Either [Net.Device] ([Net.Device], [Assertion.Assertion])))
	putStrLn ""

