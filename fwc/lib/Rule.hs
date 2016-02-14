
module Rule where

import Test.QuickCheck
import qualified MatchAction
import qualified Match
import qualified Ip
import qualified Range
import qualified List
import qualified Doc
import qualified Parse
import qualified Text.ParserCombinators.Parsec as Parsec

data Rule = Region MatchAction.Match | Rule String MatchAction.Match MatchAction.Action deriving (Show, Read, Eq, Ord)

instance Doc.Able Rule where
	repr (Region m) = Doc.hcat [ Doc.repr "{region ", (Doc.repr . tail . Doc.render) $ Doc.stru [ Doc.repr $ init $ tail $ Doc.showrepr m]]
	repr (Rule name m a) = Doc.struhcat [ Doc.hsep [ Doc.repr "rule", Doc.repr name ], Doc.repr $ init $ tail $ Doc.showrepr m, Doc.repr a ]
	name r@(Region _) = Doc.repr r
	name (Rule name _ _) = Doc.hsep [ Doc.repr "rule", Doc.repr name ]

instance Range.Rangeable Rule where
	toListOfRanges r = (Range.toListOfRanges . MatchAction.maToTuple) (getMatch r)
	fromListOfRanges r = Region (MatchAction.tupleToMatch $ Range.fromListOfRanges r)
	intersection r1 r2 = case Range.intersection m1 m2 of
		Nothing -> Nothing
		Just j -> Just $ Region j
		where
			m1 = getMatch r1
			m2 = getMatch r2
	difference r1 r2 = map ( \ a -> (Region a)) (Range.difference (getMatch r1) (getMatch r2))

instance Parse.Parseable Rule where
	parseImpl = Parse.lexeme $ do
		Parse.reserved "rule"
		name <- Parse.identifier Parsec.<?> "rule name"
		Parse.symbol ","
		match <- Parse.parse
		act <- Parse.parse
		return $ (Rule name match act)

instance Arbitrary Rule where
	arbitrary = do
		ma <- arbitrary
		act <- arbitrary
		return (Rule "rule" ma act)

getMatch :: Rule -> MatchAction.Match
getMatch (Rule _ m _) = m
getMatch (Region m) = m

getAction :: Rule -> MatchAction.Action
getAction (Rule _ _ action) = action
getAction _ = error "regions do not have action"

defaultRule :: Rule
defaultRule = Rule "default" MatchAction.defaultMA MatchAction.Deny

treeTop :: Range.Tree Rule
treeTop = Range.Leaf [defaultRule]

buildTree :: [Rule] -> Range.Tree Rule
buildTree rs = Range.buildRangeTree rs treeTop

ruleGetSrc :: Rule -> Range.Range Ip.Addr
ruleGetSrc (Rule _ (MatchAction.Match _ src _ _ _) _) = src
ruleGetSrc _ = undefined

ruleGetDst :: Rule -> Range.Range Ip.Addr
ruleGetDst (Rule _ (MatchAction.Match _ _ _ dst _) _) = dst
ruleGetDst _ = undefined

getImportantIpsTuple :: [Rule] -> ([Ip.Addr], [Ip.Addr])
getImportantIpsTuple rules = (srcs, dsts)
	where
		srcs = List.concatMap Range.importantElements (List.map ruleGetSrc rules)
		dsts = List.concatMap Range.importantElements (List.map ruleGetDst rules)

getImportantIpsRule :: Rule -> [Ip.Addr]
getImportantIpsRule rule = rv
	where
		src = ruleGetSrc rule
		dst = ruleGetDst rule
		rv = Range.importantElements src ++ Range.importantElements dst

getImportantIps :: [Rule] -> [Ip.Addr]
getImportantIps rules = concatMap getImportantIpsRule rules

