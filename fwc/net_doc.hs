
module Main where
import qualified Doc
import qualified Assertion
import qualified Net

main :: IO ()
main = do
	interact (Doc.render . Doc.repr . devs)
	putStrLn ""
	where
		devs :: String -> [Net.Device]
		devs = Assertion.smartDeviceParser

