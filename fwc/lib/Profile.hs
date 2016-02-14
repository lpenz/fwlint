
module Profile where

import qualified Data.Maybe as Maybe
import qualified Ip
import qualified Net
import qualified Range
import qualified Rule
import qualified Match
import qualified Action

build :: Net.Device -> [Range.Range Ip.Addr] -> [Range.Range Ip.Addr] -> [(Rule.Rule, Match.Match)]
build device srcs dsts = case device of
	Net.Router name _ -> map buildRuleFor srcdst
		where
			srcdst = [ (src, dst) | src <- srcs, dst <- dsts ]
			buildRuleFor :: (Range.Range Ip.Addr, Range.Range Ip.Addr) -> (Rule.Rule, Match.Match)
			buildRuleFor (src, dst) = (Rule.Rule ("router "++name) match Action.Accept, match)
				where
					match = Match.tupleToMatch (Match.protocolRangeTotal, src, Match.portRangeTotal, dst, Match.portRangeTotal)
	--Net.Filter _ _ _ -> []
	Net.Filter _ _ rules -> concatMap filtrules srcdst
		where
			srcdst = [ (src, dst) | src <- srcs, dst <- dsts ]
			disjunct = Range.buildDisjunct rules
			filtrules :: (Range.Range Ip.Addr, Range.Range Ip.Addr) -> [(Rule.Rule, Match.Match)]
			filtrules sd = Maybe.catMaybes $ map ( \ (l, r) -> case Match.maIntersectSrcDst sd (Rule.getMatch r) of
				Nothing -> Nothing 
				Just m -> Just $ (l, m)) disjunct

equivalent :: [(Rule.Rule, Match.Match)] -> [(Rule.Rule, Match.Match)] -> Bool
equivalent profile1 profile2 = size profile1 == size profile2 && (and . map equiv) [(p1, p2) | p1 <- profile1, p2 <- profile2 ]
	where
		size :: [(Rule.Rule, Match.Match)] -> Integer
		size profile = foldl ( \ i (_, m) -> i + Range.size m) 0 profile
		equiv :: ((Rule.Rule, Match.Match), (Rule.Rule, Match.Match)) -> Bool
		equiv ((r1, m1), (r2, m2)) = case Range.intersection m1 m2 of
			Just _ -> Rule.getAction r1 == Rule.getAction r2
			Nothing -> True

disagreements :: [(Rule.Rule, Match.Match)] -> [(Rule.Rule, Match.Match)] -> [(Rule.Rule, Rule.Rule, Match.Match)]
disagreements profile1 profile2 = Maybe.catMaybes $ map disagree [(p1, p2) | p1 <- profile1, p2 <- profile2 ]
	where
		disagree :: ((Rule.Rule, Match.Match), (Rule.Rule, Match.Match)) -> Maybe (Rule.Rule, Rule.Rule, Match.Match)
		disagree ((r1, m1), (r2, m2)) = case Range.intersection m1 m2 of
			Just m -> if Rule.getAction r1 == Rule.getAction r2 then Nothing else Just (r1, r2, m)
			Nothing -> Nothing

blocks :: [(Rule.Rule, Match.Match)] -> [(Rule.Rule, Match.Match)] -> ([Rule.Rule], [Match.Match])
blocks profile1 profile2 = foldr collector ([], []) results
	where
		collector :: Either (Maybe Match.Match) Rule.Rule -> ([Rule.Rule], [Match.Match]) -> ([Rule.Rule], [Match.Match])
		collector (Left Nothing) prev = prev
		collector (Left (Just m)) (rules, matchs) = (rules, m:matchs)
		collector (Right r) (rules, matchs) = (r:rules, matchs)
		results = map disagree [(p1, p2) | p1 <- profile1, p2 <- profile2 ]
			where
				disagree :: ((Rule.Rule, Match.Match), (Rule.Rule, Match.Match)) -> Either (Maybe Match.Match) Rule.Rule
				disagree ((r1, m1), (r2, m2)) = if Rule.getAction r1 == Action.Accept && Rule.getAction r2 == Action.Deny
					then Left $ Range.intersection m1 m2
					else Right r2

anyDeny :: [(Rule.Rule, Match.Match)] -> Bool
anyDeny profile = or $ map ( \ (r, _) -> Rule.getAction r == Action.Deny) profile

