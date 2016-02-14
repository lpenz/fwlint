
module AnomDistr where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Match
import qualified Range
import qualified Ip
import qualified Doc
import qualified Net
import qualified Profile
import qualified Rule
import qualified AnomIsol
import Util

data AnomDistr =
	AnomDistrDisagreement (Net.Device, Rule.Rule) (Net.Device, Rule.Rule) [Match.Match] |
	AnomDistrBlock Net.Device [Match.Match] |
	AnomDistrLeak  Net.NetAtom Net.NetAtom [Net.Device] |
	AnomDistrIrrelevancy Net.Device Rule.Rule
	deriving (Show, Ord, Eq)

instance Doc.Able AnomDistr where
	repr (AnomDistrDisagreement r1 r2 m) = Doc.stru [ Doc.repr "Disagreement", Doc.name r1, Doc.name r2, Doc.repr m ]
	repr (AnomDistrBlock d m) = Doc.stru [ Doc.repr "Block", Doc.name d, Doc.repr m ]
	repr (AnomDistrLeak n1 n2 d) = Doc.stru [ Doc.repr "Leak", Doc.repr n1, Doc.repr n2, Doc.name d ]
	repr (AnomDistrIrrelevancy d r) = Doc.stru [ Doc.repr "Irrelevant", Doc.name d, Doc.name r ]
	name (AnomDistrDisagreement r1 r2 m) = Doc.repr "Disagreement" Doc.$$ (Doc.nestup $ Doc.repr "of  " Doc.<+> Doc.name r1 Doc.$$ Doc.repr "with" Doc.<+> Doc.name r2 Doc.$$ Doc.repr "at  " Doc.<+> Doc.name m)
	name (AnomDistrBlock d m) = Doc.repr "Block in" Doc.<+> Doc.name d Doc.$$ (Doc.nestup $ Doc.repr "at" Doc.<+> (Doc.name m))
	name (AnomDistrLeak n1 n2 d) = Doc.hsep [ Doc.repr "Leak between", Doc.name n1, Doc.repr "and", Doc.name n2, Doc.repr "in path", Doc.name d ]
	name (AnomDistrIrrelevancy d r) = Doc.hsep [ Doc.repr "Irrelevant", Doc.name r, Doc.name "in", Doc.name d ]

ipnetrangeDevs :: [Net.Device] -> Net.NetAtom -> [Range.Range Ip.Addr]
ipnetrangeDevs _ (Net.NetAtom _ ip pl) = [Ip.getIpNetRange ip pl]
ipnetrangeDevs devs (Net.NetAtomInternet) = Net.getInternetNetworks devs

filterAnoms :: [Net.Device] -> [AnomDistr] -> [AnomDistr]
filterAnoms devs anoms0 = disagreesfiltered ++ blocksfiltered ++ other
	where
		(disagrees, blocks, other) = foldr splitAnoms ([], [], []) anoms0
		splitAnoms a@(AnomDistrDisagreement _ _ _) (d, b, o) = (d ++ [a], b, o)
		splitAnoms a@(AnomDistrBlock _ _) (d, b, o) = (d, b ++ [a], o)
		splitAnoms a (d, b, o) = (d, b, o ++ [a])

		disagreesfiltered = map tuple2disa (joinTupleList disatuples)
		disa2tuple (AnomDistrDisagreement (d1, r1) (d2, r2) ms) = ((d1, r1, d2, r2), ms)
		disa2tuple _ = error "not a disagrement"
		disatuples = map disa2tuple disagrees
		tuple2disa ((d1, r1, d2, r2), ms) = AnomDistrDisagreement (d1, r1) (d2, r2) ms
		disaregs = concatMap snd disatuples

		disadiff :: Match.Match -> [Match.Match]
		disadiff r = foldr folder [r] disaregs
			where
				folder :: Match.Match -> [Match.Match] -> [Match.Match]
				folder dis as = concatMap ( \ a -> Range.difference a dis) as
		blockstuple = map block2tuple blocks where
			block2tuple (AnomDistrBlock d ms) = (d, ms)
			block2tuple _ = error "not a block"
		filtnullsnd = filter (not . null . snd)
		blockstuplefiltered1 = filtnullsnd ( map ( \ (d, ms) -> (d, filter ( \ m -> not $ Net.matchOnOneNetwork devs m) ms)) blockstuple )
		blockstuplefiltered2 = filtnullsnd ( map ( \ (d, ms) -> (d, concatMap disadiff ms)) blockstuplefiltered1 )
		blockstuplejoined = joinTupleList blockstuplefiltered2
		blocksfiltered = map tuple2block blockstuplejoined
		tuple2block (d, ms) = AnomDistrBlock d ms


type GlobalProfile = Map.Map (Net.NetAtom, Net.NetAtom) (Net.Device, [(Rule.Rule, Match.Match)])
type StateOfBuildProfile = (GlobalProfile, [AnomDistr], Set.Set (Net.Device, Rule.Rule))

buildGlobalProfileAnomDistr :: [Net.Device] -> [Net.Path] -> StateOfBuildProfile
buildGlobalProfileAnomDistr devices pathsTop = (globalprofile0, anoms0, usedrules0)
	where
		(globalprofile0, anoms1, usedrules0) = foldr buildProfiles (Map.empty, [], Set.empty) pathsTop
		anoms0 = filterAnoms devices anoms1
		buildProfiles ::
			Net.Path
			-> StateOfBuildProfile
			-> StateOfBuildProfile
		buildProfiles (ns1, path, ns2) stateBuildProfile =
			foldr buildProfileForNets stateBuildProfile devnets
			where
				ipnetrange = ipnetrangeDevs devices
				devnets = [(src, dst) | src <- ns1, dst <- ns2 ]
				extremes = Net.findExtremeFilters path
				(d1, d2) = Maybe.fromJust extremes
				buildProfileForNets ::
					(Net.NetAtom, Net.NetAtom)
					-> StateOfBuildProfile
					-> StateOfBuildProfile
				buildProfileForNets nets@(n1, n2) (globprof, anoms, usedrules) =
					if extremes == Nothing
						then (Map.insertWith const2 nets (router0, Profile.build router0 nr1 nr2) globprof, anoms, usedrules)
						else rvWithFilters
					where
						router0 = path !! 0
						dis dl1 dl2 pr1 pr2 = (Maybe.mapMaybe ( \ (r1, r2, m) -> if Net.matchOnOneNetwork devices m then Nothing else Just $ AnomDistrDisagreement (dl1, r1) (dl2, r2) [m])) $ Profile.disagreements pr1 pr2
						(d0, p0) = Map.findWithDefault (d1, p1) nets globprof
						nr1 = ipnetrange n1
						nr2 = ipnetrange n2
						p1 = Profile.build d1 nr1 nr2
						p2 = Profile.build d2 nr1 nr2
						usedof d p = Maybe.mapMaybe ( \ (r, _) -> if (Net.matchOnOneNetwork devices (Rule.getMatch r)) then Nothing else Just (d, r)) p
						newusedrules = Set.union usedrules (Set.fromList (
							usedof d1 p1
							++ usedof d2 p2
							++ blockusedrules
							))
						rvWithFilters = if (Map.member nets globprof && Net.isFilter (fst $ globprof Map.! nets))
							then (globprof, anoms ++ dis d0 d1 p0 p1 ++ dis d0 d2 p0 p2 ++ blocks, newusedrules)
							else (Map.insert nets (d1, p1) globprof, anoms ++ dis d1 d2 p1 p2 ++ blocks, newusedrules)
						(blocks, blockusedrules) = case path of 
							(_:_:_) -> foldr ( \ (na, nb) (pa, pb) -> (na ++ pa, nb ++ pb) ) ([], []) (map findBlocks ((init . tail) path))
							_ -> ([], [])
						findBlocks :: Net.Device -> ([AnomDistr], [(Net.Device, Rule.Rule)])
						findBlocks dev = (map ( \ m -> AnomDistrBlock dev [m]) blks, map ( \ r -> (dev, r) ) used)
							where
								pn = Profile.build dev nr1 nr2
								(used, blks) = Profile.blocks p0 pn

check :: [Net.Device] -> [AnomDistr]
check devices = anomsDisagreeBlock ++ anomsLeak ++ anomsIrrelevant
	where
		pathsTop = Net.allPaths devices
		(globalprofile, anomsDisagreeBlock, usedrules) = buildGlobalProfileAnomDistr devices pathsTop
		anomsLeak = concatMap findLeaksOfPath pathsTop
			where
				findLeaksOfPath ::
					Net.Path
					-> [AnomDistr]
				findLeaksOfPath (ns1, path, ns2) =
					if extremes == Nothing
						then concatMap findLeaksForNet devnets
						else []
					where
						devnets = [(src, dst) | src <- ns1, dst <- ns2, src < dst ]
						extremes = Net.findExtremeFilters path
						findLeaksForNet ::
							(Net.NetAtom, Net.NetAtom)
							-> [AnomDistr]
						findLeaksForNet nets@(n1, n2) = rv
							where
								profile = if Map.notMember nets globalprofile then (error $ Doc.showrepr $ Doc.repr nets) else (globalprofile Map.! nets)
								rv = if Profile.anyDeny (snd profile) then [AnomDistrLeak n1 n2 path] else []
		anomsIrrelevant = concatMap findIrrelevantRules devices
			where
				findIrrelevantRules ::
					Net.Device
					-> [AnomDistr]
				findIrrelevantRules (Net.Router _ _) = []
				findIrrelevantRules device@(Net.Filter _ _ rules) = map ( \ r -> AnomDistrIrrelevancy device r) irrelevantrules
					where
						irrelevantrules = filter ( \ r -> Set.notMember (device, r) usedrules ) rules

filterIsol :: [Net.Path] -> [Net.Device] -> Net.Device -> [AnomIsol.AnomIsol] -> [AnomIsol.AnomIsol]
filterIsol paths devs dev anoms0 = filter filteranoms anoms0
	where
		filteranoms (AnomIsol.AnomIsolConflict matchs _ _) = not . null $ newmatchs
			where
				newmatchs = filter (not . (Net.devInPathOfMatch paths devs dev)) matchs
		filteranoms _ = True
		-- sameItf :: Match.Match -> Bool
		-- sameItf match = 
 		-- 	(head itfs == Nothing) ||
 		-- 	(homogenous $ itfs)
		-- 	where
		-- 		itfs = maGetItf match
		-- maGetItf match = itfs
		-- 	where
		-- 		(src, dst) = Match.maGetIps match
		-- 		itfs = map (Net.ipFindDevInterface [dev]) ips
		-- 		ips :: [Ip.Addr]
		-- 		ips = [
		-- 			Range.iniOfRange src,
		-- 			Range.iniOfRange dst,
		-- 			Range.fimOfRange src,
		-- 			Range.fimOfRange dst
		-- 			]

