
module Assertion where

import qualified Range
import qualified Doc
import qualified Parse
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Net
import qualified AnomDistr
import qualified Match
import qualified Action
import qualified Map
import qualified Rule
import qualified List
import qualified Data.Maybe as Maybe
import Util

data Assertion = Assertion String Match.Match Action.Action deriving (Show, Read, Eq, Ord)

instance Doc.Able Assertion where
	repr (Assertion name m a) = Doc.stru [ Doc.hcat [ Doc.repr "assert ", Doc.repr name ], Doc.repr $ init $ tail $ Doc.showrepr m, Doc.repr a ]
	name (Assertion name _ _) = Doc.repr name

instance Parse.Parseable Assertion where
	parseImpl = Parse.lexeme $ do
		Parse.reserved "assert"
		name <- Parse.identifier Parsec.<?> "rule name"
		Parse.symbol ","
		match <- Parse.parse
		act <- Parse.parse
		return $ (Assertion name match act)

getMatch :: Assertion -> Match.Match
getMatch (Assertion _ m _) = m

getAction :: Assertion -> Action.Action
getAction (Assertion _ _ a) = a

checkAssertion :: [Net.Device] -> AnomDistr.GlobalProfile -> Assertion -> [Match.Match]
checkAssertion devs globalprofile (Assertion _ match action) = errors
	where
		src = fst $ Match.maGetSrc match
		dst = fst $ Match.maGetDst match
		keys = (filter ( \ (s, d) -> Net.ipRangeInNetAtom devs src s && Net.ipRangeInNetAtom devs dst d) (Map.keys globalprofile))
		devices = List.nub $ map (fst . ((Map.!) globalprofile)) keys
		checkDevice :: Net.Device -> [Match.Match]
		checkDevice device = wrongmatches
			where
				rules = Range.getActiveFor (Net.getRules device) (Rule.Region match)
				wrongmatches = Maybe.mapMaybe ( \ r -> if Rule.getAction r == action then Nothing else Range.intersection match (Rule.getMatch r)) rules
		errors = filter (not . Net.matchOnOneNetwork devs) (concatMapUniq checkDevice devices)

checkAssertions :: [Net.Device] -> [Assertion] -> [(Assertion, [Match.Match])]
checkAssertions devs asserts = filter (not . null . snd) (map ( \ a -> (a, checkAssertion devs globalprofile a)) asserts)
	where
		(globalprofile, _, _) = AnomDistr.buildGlobalProfileAnomDistr devs $ Net.allPaths devs

checkAssertionsContradiction :: [Assertion] -> [(Assertion, Assertion)]
checkAssertionsContradiction asserts = filter filt combs
	where
		combs = [(a, b) | a <- asserts, b <- asserts, a < b]
		filt (a, b) = getAction a /= getAction b && Maybe.isJust (Range.intersection (getMatch a) (getMatch b))

smartDeviceParser :: String -> [Net.Device]
smartDeviceParser str = case (Parse.runParser :: String -> Either [Net.Device] ([Net.Device], [Assertion.Assertion])) str of
	Left a -> Net.validate a
	Right (a, _) -> Net.validate a

smartDeviceAssertionParser :: String -> ([Net.Device], [Assertion.Assertion])
smartDeviceAssertionParser str = case (Parse.runParser :: String -> Either [Net.Device] ([Net.Device], [Assertion.Assertion])) str of
	Left a -> (Net.validate a, [])
	Right (a, b) -> (Net.validate a, b)

