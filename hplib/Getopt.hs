
module Getopt (compileOpts, interactOnArgs, module System.Console.GetOpt) where

import System
import System.Console.GetOpt

compileOpts :: String -> [String] -> [OptDescr a] -> IO ([a], [String])
compileOpts extra argv options = do
	name <- getProgName
	case getOpt Permute options argv of
		(o,n,[]) -> return (o,n)
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ name ++ " [OPTIONS...] " ++ extra) options))

interactOnArgs :: [String] -> (String -> String) -> IO ()
interactOnArgs [] func = interact func
interactOnArgs list func = do
	strin <- fmap concat (mapM readFile list)
	putStrLn $ func strin


