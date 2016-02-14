
module AnomIsol where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Match
import qualified Rule
import qualified Doc
import qualified Range
import qualified Data.Maybe as Maybe

data AnomIsol =
	AnomIsolInvis    Rule.Rule
	| AnomIsolConflict [Match.Match] Rule.Rule Rule.Rule
	| AnomIsolRedundant Rule.Rule
	deriving (Show, Ord, Eq)

instance Doc.Able AnomIsol where
	repr (AnomIsolInvis rule) = Doc.stru [ Doc.repr "Invisible", Doc.repr rule ]
	repr (AnomIsolConflict _ r1 r2) = Doc.stru [ Doc.repr "Conflict", Doc.repr r1, Doc.repr r2 ]
	repr (AnomIsolRedundant rule) = Doc.stru [ Doc.repr "Redundant", Doc.repr rule ]
	name (AnomIsolInvis rule) = Doc.hsep [ Doc.repr "Invisible", Doc.name rule ]
	name (AnomIsolConflict m r1 r2) = Doc.repr "Conflict" Doc.<+> Doc.name r1 Doc.<+> Doc.name r2 Doc.$$ (Doc.nestup $ Doc.repr "at" Doc.<+> Doc.name m)
	name (AnomIsolRedundant rule) = Doc.hsep [ Doc.repr "Redundant", Doc.name rule ]

visibleRules :: [Rule.Rule] -> [[Rule.Rule]] -> [Rule.Rule]
visibleRules rs equivs = filter ( \ r -> Set.member r visibleSet) rs
	where
		visibleSet = Set.fromList $ map ( \ r -> r !! 0) equivs

checkInvisibility :: [Rule.Rule] -> [Rule.Rule] -> [AnomIsol]
checkInvisibility rules visible = map AnomIsolInvis invisible
	where
		invisible = filter ( \ r -> notElem r visible) rules

checkConflict :: [([Rule.Rule], [Rule.Rule])] -> [AnomIsol]
checkConflict equivs = List.nub $ List.sort $ concatMap checkConflictsFilter equivs

checkConflictsFilter :: ([Rule.Rule], [Rule.Rule]) -> [AnomIsol]
checkConflictsFilter (region, rs) = Maybe.catMaybes $ map (checkConflictRules (head rs)) (tail rs)
	where
		rulesConflict :: Rule.Rule -> Rule.Rule -> Bool
		rulesConflict top r =
			Rule.getAction top /= Rule.getAction r &&
			Range.relation top r /= Range.CONTAINED
		checkConflictRules :: Rule.Rule -> Rule.Rule -> Maybe AnomIsol
		checkConflictRules top r = if rulesConflict top r then (Just $ AnomIsolConflict (map Rule.getMatch region) top r) else Nothing

checkRedundancies :: [Rule.Rule] -> [([Rule.Rule], [Rule.Rule])] -> [AnomIsol]
checkRedundancies filt equivs = map AnomIsolRedundant irrelevants
	where
		topDefined :: ([Rule.Rule], [Rule.Rule]) -> Bool
		topDefined (regs, top:rs) = if null rs || (not $ null $ checkConflictsFilter (regs, rs))
			then True
			else Rule.getAction top /= Rule.getAction (head rs)
		topDefined (_, []) = error "No rule left"
		definingRules :: Set.Set Rule.Rule
		definingRules = Set.fromList $ map (head . snd) $ filter topDefined equivs
		irrelevants = filter ( \ r -> Set.notMember r definingRules) filt

checkUsing :: [Rule.Rule] -> [([Rule.Rule], [Rule.Rule])] -> [Rule.Rule] -> [AnomIsol]
checkUsing rs equivs visible = concat [
	checkInvisibility rs visible
	,checkConflict equivs
	,checkRedundancies visible equivs
	]

check :: [Rule.Rule] -> [AnomIsol]
check rs = checkUsing rs equivs visible
	where
		visible = visibleRules rs (Range.buildEquivalent $ Rule.buildTree rs)
		equivs = Range.buildEquivalentWithRegion $ Rule.buildTree visible

checkFiltering :: [Rule.Rule] -> (([Rule.Rule], [Rule.Rule]) -> Bool) -> [AnomIsol]
checkFiltering rs func = checkUsing rs equivs visible
	where
		visible = visibleRules rs (Range.buildEquivalent $ Rule.buildTree rs)
		equivs = filter func $ Range.buildEquivalentWithRegion $ Rule.buildTree visible

