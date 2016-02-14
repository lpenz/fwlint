
module Main where
import qualified Assertion
import qualified Net
import NetDot

main :: IO ()
main = do
	interact (getDotFromDevs . devs)
	putStrLn ""
	where
		devs :: String -> [Net.Device]
		devs = Assertion.smartDeviceParser

