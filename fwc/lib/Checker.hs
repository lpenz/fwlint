
module Checker where

import qualified Net
import qualified AnomIsol
import qualified AnomDistr
import qualified Assertion
import qualified Doc
import qualified Match

data Error = ErrorAnomIsol (Net.Device, [AnomIsol.AnomIsol])
	| ErrorAnomDistr AnomDistr.AnomDistr
	| ErrorAssertionHold Assertion.Assertion [Match.Match]
	| ErrorAssertionContradiction Assertion.Assertion Assertion.Assertion
	deriving (Eq, Show)

instance Doc.Able Error

isDistr :: Error -> Bool
isDistr (ErrorAnomIsol _) = False
isDistr _ = True

show :: [Error] -> String
show [] = "No anomalies or errors found\n"
show errors = concatMap shower errors
	where
		shower (ErrorAnomDistr a) = Doc.showname a ++ "\n"
		shower (ErrorAnomIsol (d, a)) = Net.getName d ++ ":\n" ++ concatMap shower2 a
			where shower2 s = "  " ++ Doc.showname s ++ "\n"
		shower (ErrorAssertionHold (Assertion.Assertion name _ _) ms) = Doc.showrepr ( Doc.repr "Assertion" Doc.<+> Doc.repr name Doc.<+> Doc.repr " violated at" Doc.$$ (Doc.nestup $ Doc.name ms)) ++ "\n"
		shower (ErrorAssertionContradiction (Assertion.Assertion name1 _ _) (Assertion.Assertion name2 _ _)) = "Assertions contradiction: " ++ name1 ++ " and " ++ name2 ++ "\n"

check :: ([Net.Device], [Assertion.Assertion]) -> [Error]
check (devices, assertions) = isol ++ distr ++ assertsincompatible ++ assertshold
	where
		paths = Net.allPaths devices
		isol0 = map ( \ d -> ErrorAnomIsol (d, AnomDistr.filterIsol paths devices d (AnomIsol.check $ Net.getRules d)) ) devices 
		isol = filter f isol0
		f (ErrorAnomIsol (_, [])) = False
		f _ = True
		distr = map ErrorAnomDistr (AnomDistr.check devices)
		assertsincompatible = map (uncurry ErrorAssertionContradiction) (Assertion.checkAssertionsContradiction assertions)
		assertshold = map ( \ (a, ms) -> ErrorAssertionHold a ms) (Assertion.checkAssertions devices assertions)

checkEither :: Either [Net.Device] ([Net.Device], [Assertion.Assertion]) -> [Error]
checkEither (Left devs) = check (devs, [])
checkEither (Right b) = check b

