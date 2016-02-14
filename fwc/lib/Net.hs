
{-# LANGUAGE FlexibleInstances #-}

module Net where

import Test.QuickCheck
import qualified Ip
import qualified Rule
import qualified Doc
import qualified Parse
import qualified Range
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Map
import qualified Match
import qualified Doc

data Interface = Interface Ip.Addr NetAtom deriving (Show, Ord, Eq)

instance Doc.Able Interface where
	repr (Interface ip (NetAtom _ _ pl)) = Doc.stru [ Doc.repr (ip, pl) ]
	repr (Interface _ (NetAtomInternet)) = Doc.repr "{internet}"
	name (Interface _ (NetAtom name _ _)) = Doc.repr name
	name (Interface _ (NetAtomInternet)) = Doc.repr $ "internet"

instance Parse.Parseable Interface where
	parseAliased = Parse.lexeme $ do
		(m, _) <- Parsec.getState
		key <- Parse.identifier
		if Map.notMember key m
			then fail $ "key " ++ show key ++ " not found"
			else return $ mknewiface key (Parse.runAliasParser $ m Map.! key)
				where
					mknewiface name (Interface ip (NetAtom _ _ pl)) = Interface ip (getNetAtom name ip pl)
					mknewiface _ (Interface ip (NetAtomInternet)) = Interface ip (NetAtomInternet)
	parseImpl = Parse.lexeme $ do
		Parsec.try parseInterfaceIp Parsec.<|> parseInterfaceInternet
		where
			parseInterfaceIp = do
				(m, _) <- Parsec.getState
				(ip, pl) <- Parse.parse
				return $ parseInterfaceIpRet m ip pl
			parseInterfaceIpRet m ip pl = Interface ip (getNetAtom name ip pl)
				where
					mp = Map.invert m
					key = Doc.showrepr (Ip.getIpNetMinIp ip pl, pl)
					name = if Map.notMember key mp
						then ""
						else mp Map.! key
			parseInterfaceInternet = do
				Parse.reserved "internet"
				return $ Interface (Ip.makeAddr 0 0 0 0) NetAtomInternet

instance Arbitrary Interface where
	arbitrary = do
		ip <- arbitrary
		pl <- choose (0, 32)
		return $ Interface ip (getNetAtom "net" ip pl)

------

data NetAtom = NetAtom String Ip.Addr Int | NetAtomInternet deriving (Show, Ord, Eq)

instance Doc.Able NetAtom where
	repr (NetAtom _ ip pl) = Doc.stru [ Doc.repr (ip, pl) ]
	repr (NetAtomInternet) = Doc.repr "{internet}"
	name n@(NetAtom "" _ _) = Doc.repr n
	name (NetAtom name _ _) = Doc.repr name
	name (NetAtomInternet) = Doc.repr "internet"

instance Parse.Parseable NetAtom where
	parseImpl = Parse.lexeme $ do
		Parse.reserved "network"
		name <- Parse.identifier
		ip <- Parse.parse
		Parse.symbol "/"
		pl <- Parse.parse
		return $ NetAtom name ip (fromInteger pl)

getNetAtom :: String -> Ip.Addr -> Int -> NetAtom
getNetAtom name ip0 pl = NetAtom name ip pl
	where
		(ip, _) = Ip.fixIpNet ip0 pl

getNetAtomNoName :: Ip.Addr -> Int -> NetAtom
getNetAtomNoName ip0 pl = NetAtom "" ip pl
	where
		(ip, _) = Ip.fixIpNet ip0 pl

getNetAtomName :: NetAtom -> String
getNetAtomName (NetAtom "" ip pl) = Doc.showrepr (Ip.getIpNetMinIp ip pl, pl)
getNetAtomName (NetAtom name _ _) = name
getNetAtomName (NetAtomInternet) = "internet"

------

data Device = Filter String [Interface] [Rule.Rule] | Router String [Interface] deriving (Show, Eq, Ord)

instance Doc.Able Device where
	repr (Filter name interfaces [])    = Doc.stru [ Doc.hsep [ Doc.repr "filter", Doc.repr name ], Doc.repr interfaces ]
	repr (Filter name interfaces rules) = Doc.stru [ Doc.hsep [ Doc.repr "filter", Doc.repr name ], Doc.repr interfaces, Doc.repr rules ]
	repr (Router name interfaces)       = Doc.stru [ Doc.hsep [ Doc.repr "router", Doc.repr name ], Doc.repr interfaces ]
	name (Filter name _ _)  = Doc.hsep [ Doc.repr "filter", Doc.repr name ]
	name (Router name _)    = Doc.hsep [ Doc.repr "router", Doc.repr name ]

instance Parse.Parseable Device where
	parseImpl = Parse.lexeme $ do
		Parsec.try routr
		Parsec.<|> Parsec.try filtr1
		Parsec.<|> filtr2
		where
			routr = do
				Parse.reserved "router"
				name <- Parse.identifier Parsec.<?> "router name"
				Parse.symbol ","
				interfaces <- Parse.parse
				return $ Router name interfaces
			filtr1 = do
				Parse.reserved "filter"
				name <- Parse.identifier Parsec.<?> "filter name"
				Parse.symbol ","
				interfaces <- Parse.parse
				Parse.symbol ","
				rules <- Parse.parse
				return $ Filter name interfaces rules
			filtr2 = do
				Parse.reserved "filter"
				name <- Parse.identifier Parsec.<?> "filter name"
				Parse.symbol ","
				interfaces <- Parse.parse
				return $ Filter name interfaces []

instance Doc.Able (Device, Rule.Rule) where
	repr t = Doc.repr t
	name (f, r) = Doc.hcat [ Doc.name f, Doc.repr ", ",  Doc.name r ]

instance Arbitrary Device where
	arbitrary = do
		interfaces <- arbitrary
		rules <- arbitrary
		oneof [return $ Filter "test" interfaces rules, return $ Router "test" interfaces]

checkDevices :: [Device] -> Bool
checkDevices devs = rv
	where
		na = map snd $ getAllNetAtoms devs
		nacross = [ (na1, na2) | na1 <- na, na2 <- na, na1 < na2 ]
		rv = and $ map checknas nacross
		checknas :: (Net.NetAtom, Net.NetAtom) -> Bool
		checknas (na1, na2) = case (nar1, nar2) of
			(Nothing, _) -> True
			(_, Nothing) -> True
			(Just n1, Just n2) -> Range.relation n1 n2 == Range.DISJUNCT || Range.relation n1 n2 == Range.EQUAL
			where
				nar1 = Maybe.listToMaybe $ getNetAtomIpRanges na1
				nar2 = Maybe.listToMaybe $ getNetAtomIpRanges na2

------

type Path = ([NetAtom], [Device], [NetAtom])

------

isFilter :: Device -> Bool
isFilter (Filter _ _ _) = True
isFilter _ = False

getName :: Device -> String
getName (Router name _)   = name
getName (Filter name _ _) = name

getInterfaces :: Device -> [Interface]
getInterfaces (Router _ interfaces)   = interfaces
getInterfaces (Filter _ interfaces _) = interfaces

getRules :: Device -> [Rule.Rule]
getRules (Router _ _)   = []
getRules (Filter _ _ rules) = rules

getNetworkOfInterface :: Interface -> NetAtom
getNetworkOfInterface (Interface _ n) = n

getNetworks :: Device -> [NetAtom]
getNetworks = map getNetworkOfInterface . getInterfaces

getNetworkNameMap :: [Device] -> Map.Map (Ip.Addr, Int) String
getNetworkNameMap devs = Map.fromList $ map totuple (filter netIsNotInternet $ concatMap getNetworks devs)
	where
		netIsNotInternet (NetAtomInternet) = False
		netIsNotInternet _ = True
		totuple (NetAtom name ip pl) = ((ip, pl), name)
		totuple _ = undefined

netAtomIsInternet :: NetAtom -> Bool
netAtomIsInternet (NetAtomInternet) = True
netAtomIsInternet _ = False

interfaceIsInternet :: Interface -> Bool
interfaceIsInternet (Interface _ NetAtomInternet) = True
interfaceIsInternet _ = False

deviceHasInternet :: Device -> Bool
deviceHasInternet device = or $ map interfaceIsInternet $ getInterfaces device

onSameNetwork :: Interface -> Interface -> Bool
onSameNetwork i1@(Interface _ (NetAtom _ _ _)) i2@(Interface _ (NetAtom _ _ _)) = getNetworkOfInterface i1 == getNetworkOfInterface i2
onSameNetwork _ _ = False

connection :: Device -> Device -> [NetAtom]
connection d1 d2 = if d1 == d2 then [] else rv
	where
		d1interfaces = getInterfaces d1
		d2interfaces = getInterfaces d2
		rv = List.nub $ [ getNetworkOfInterface a | a <- d1interfaces, b <- d2interfaces, onSameNetwork a b ]

neighboors :: [Device] -> Device -> [(Device, [NetAtom])]
neighboors devices dev = filter ( \ t -> not $ null $ snd t) (map ( \ d -> (d, connection d dev)) devices)

allpathsfrom :: [Device] -> Device -> [Path]
allpathsfrom devices deviceSourceTop = (getNetworks deviceSourceTop, [deviceSourceTop], getNetworks deviceSourceTop):(adapt $ allpathsdfs (Set.fromList [deviceSourceTop]) (Set.empty) deviceSourceTop)
	where
		adapt :: [(Set.Set NetAtom, [Device], Set.Set NetAtom)] -> [Path]
		adapt list = map ( \ (s1, d, s2) -> (Set.toList s1, d, Set.toList s2)) list
		allpathsdfs :: Set.Set Device -> Set.Set NetAtom -> Device -> [(Set.Set NetAtom, [Device], Set.Set NetAtom)]
		allpathsdfs devstack netstack deviceSource = rv
			where
				connsplain :: [(Device, NetAtom)]
				connsplain = concatMap ( \ (d, ns) -> map ( \ n -> (d, n)) ns) (neighboors devices deviceSource)
				connsfiltered :: [(Device, NetAtom)]
				connsfiltered = filter ( \ (d, n) -> Set.notMember d devstack && Set.notMember n netstack) connsplain
				networksSource = Set.fromList $ getNetworks deviceSource
				rv :: [(Set.Set NetAtom, [Device], Set.Set NetAtom)]
				rv = concatMap ( \ (d, n) -> allpathsfrom2 deviceSource d n) connsfiltered
					where
						allpathsfrom2 :: Device -> Device -> NetAtom
							-> [(Set.Set NetAtom, [Device], Set.Set NetAtom)]
						allpathsfrom2 d0 d n = (netsource, [d0, d], netdest):
							(map ( \ (_, l, b) -> (netsource, d0:l, b)) (allpathsdfs newdevstack newnetstack d))
							where
								netsource = Set.delete n networksSource
								netdest = Set.delete n (Set.fromList $ getNetworks d)
								newdevstack = Set.insert d devstack
								newnetstack = Set.insert n netstack

terminalNetworks :: [Device] -> Set.Set NetAtom
terminalNetworks devs = fst $ foldr devFolder (Set.empty, Set.empty) devs
	where
		devFolder :: Device -> (Set.Set NetAtom, Set.Set NetAtom) -> (Set.Set NetAtom, Set.Set NetAtom)
		devFolder dev s = foldr netFolder s (getNetworks dev)
		netFolder :: NetAtom -> (Set.Set NetAtom, Set.Set NetAtom) -> (Set.Set NetAtom, Set.Set NetAtom)
		netFolder net (terminals, inners) = if Set.member net inners
			then (terminals, inners)
			else if Set.member net terminals
				then (Set.delete net terminals, Set.insert net inners)
				else (Set.insert net terminals, inners)

allPaths :: [Device] -> [Path]
allPaths devices = concatMap ( \ d -> allpathsfrom devices d) devices

sortPaths :: [Path] -> [Path]
sortPaths = List.sortBy sorter
	where
		sorter :: Path -> Path -> Ordering
		sorter (_, l1, _) (_, l2, _) = case compare (head l1) (head l2) of
			EQ -> compare (tail l1) (tail l2)
			a -> a

groupPaths :: [Path] -> [[Path]]
groupPaths = List.groupBy grouper . sortPaths
	where
		grouper :: Path -> Path -> Bool
		grouper (_, l1, _) (_, l2, _) = (head l1) == (head l2) && (tail l1) == (tail l2)

pathHasOnlyRouters :: [Device] -> Bool
pathHasOnlyRouters ((Filter _ _ _):[]) = True
pathHasOnlyRouters ((Filter _ _ _):ds) = True && pathHasOnlyRouters ds
pathHasOnlyRouters _ = False

findExtremeFilters :: [Device] -> Maybe (Device, Device)
findExtremeFilters filts = if null ff then Nothing else Just (firstfilter filts, firstfilter (reverse filts))
	where
		ff = dropWhile (not . isFilter) filts
		firstfilter = head . (dropWhile (not . isFilter))

getNetAtomIpRanges :: NetAtom -> [Range.Range Ip.Addr]
getNetAtomIpRanges (NetAtom _ ip pl) = [Ip.getIpNetRange ip pl]
getNetAtomIpRanges (NetAtomInternet) = []

ipaddrInNetAtom :: [Device] -> Ip.Addr -> NetAtom -> Bool
ipaddrInNetAtom _ ip (NetAtom _ naip napl) = Range.matchRange (Ip.getIpNetRange naip napl) ip
ipaddrInNetAtom devs ip (NetAtomInternet) = or $ map ( \ r -> Range.matchRange r ip ) (getInternetNetworks devs)

ipRangeInNetAtom :: [Device] -> Range.Range Ip.Addr -> NetAtom -> Bool
ipRangeInNetAtom _ ip (NetAtom _ naip napl) = Maybe.isJust $ Range.intersection (Ip.getIpNetRange naip napl) ip
ipRangeInNetAtom devs ip (NetAtomInternet) = or $ map ( \ r -> Maybe.isJust $ Range.intersection r ip ) (getInternetNetworks devs)

ipaddrInInternet :: [Device] -> Ip.Addr -> Bool
ipaddrInInternet devs ip = not $ or $ map ( \ r -> Range.matchRange r ip ) ipranges
	where
		interfaces = concatMap getNetworks devs
		ipranges = concatMap getNetAtomIpRanges interfaces

getInternetNetworks :: [Device] -> [Range.Range Ip.Addr]
getInternetNetworks devs = foldr folder [Ip.addrRangeTotal] ipranges
	where
		interfaces = concatMap getNetworks devs
		ipranges = concatMap getNetAtomIpRanges interfaces
		folder :: Range.Range Ip.Addr -> [Range.Range Ip.Addr] -> [Range.Range Ip.Addr]
		folder ipr prev = concatMap ( \ a -> Range.differenceOfRange a ipr ) prev

getNetAtomIpRangeSmart :: [Device] -> NetAtom -> [Range.Range Ip.Addr]
getNetAtomIpRangeSmart _ na@(NetAtom _ _ _) = getNetAtomIpRanges na
getNetAtomIpRangeSmart devs _ = getInternetNetworks devs

getImportantIps :: [Device] -> [Ip.Addr]
getImportantIps devs = Set.toList ips ++ interfaceips
	where
		allrules = concatMap getRules devs
		ips = foldr folder Set.empty allrules
		folder :: Rule.Rule -> Set.Set Ip.Addr -> Set.Set Ip.Addr
		folder rule ipset0 = foldr Set.insert ipset0 (Rule.getImportantIps [rule])
		interfaceips = concatMap getFirstAndLast (concatMap getNetAtomIpRanges (concatMap getNetworks devs))
		getFirstAndLast :: Range.Range Ip.Addr -> [Ip.Addr]
		getFirstAndLast range = Range.importantElementsOfRange range

ipFindDevInterface :: [Net.Device] -> Ip.Addr -> Maybe (Net.Device, Interface)
ipFindDevInterface devs ip0 = rv
	where
		options = concatMap ( \ d -> map ( \ i -> (d, i) ) (getInterfaces d) ) devs
		rv = if ipaddrInInternet devs ip0 then firstinternet else ipfind
		firstinternet = Maybe.listToMaybe $ filter internetFilter options
		internetFilter (_, (Interface _ (NetAtomInternet))) = True
		internetFilter _ = False
		ipfind = Maybe.listToMaybe $ filter ipfinder options
		ipfinder (_, (Interface _ na@(NetAtom _ _ _))) = ipaddrInNetAtom devs ip0 na
		ipfinder _ = False

ipsOnSameNetworkDevs :: [Net.Device] -> Ip.Addr -> Ip.Addr -> Bool
ipsOnSameNetworkDevs devs ip1 ip2 = rv
	where
		rv = ipFindDevInterface devs ip1 == ipFindDevInterface devs ip2

getAllNetAtoms :: [Net.Device] -> [(Net.Device, Net.NetAtom)]
getAllNetAtoms devs = concatMap ( \ d -> map ( \ i -> (d, getNetworkOfInterface i) ) (getInterfaces d) ) devs

matchNetworks :: [Net.Device] -> Match.Match -> [(Net.Device, Net.NetAtom)]
matchNetworks devs match = rv
	where
		rv = Maybe.mapMaybe ( \ (d, na) -> if (netAtomIsInternet na || (not $ checkna na)) then Nothing else Just (d,na) ) (getAllNetAtoms devs)
		checkna :: Net.NetAtom -> Bool
		checkna na = (rsrc /= Range.DISJUNCT) && (rdst /= Range.DISJUNCT)
			where
				rsrc = Range.relation narange src
				rdst = Range.relation narange dst
				(src, dst) = Match.maGetIps match
				narange = head $ getNetAtomIpRanges na

matchOnOneNetwork :: [Net.Device] -> Match.Match -> Bool
matchOnOneNetwork devs match = inside
	where
		options = Maybe.mapMaybe ( \ (_, na) -> if netAtomIsInternet na then Nothing else Just na ) (getAllNetAtoms devs)
		inside = or $ map checkit options
		checkit na = (r1 == Range.EQUAL || r1 == Range.CONTAINS) && (r2 == Range.EQUAL || r2 == Range.CONTAINS)
			where
				r1 = Range.relation narange src
				r2 = Range.relation narange dst
				(src, dst) = Match.maGetIps match
				narange = head $ getNetAtomIpRanges na

pathsExplode :: [Path] -> [(NetAtom, [Device], NetAtom)]
pathsExplode = concatMap pathExplode
	where
		pathExplode (nets1, devs, nets2) = [(net1, devs, net2) | net1 <- nets1, net2 <- nets2, net1 /= net2]

devInPathOfMatch :: [Path] -> [Net.Device] -> Net.Device -> Match.Match -> Bool
devInPathOfMatch paths0 devs0 dev match = (null pathsmatchWithDev)
	where
		pathsexploded = pathsExplode paths0
		pathsmatch = filter pathHasMatchQ pathsexploded
		(msrc, mdst) = Match.maGetIps match
		pathHasMatchQ :: (NetAtom, [Device], NetAtom) -> Bool
		pathHasMatchQ (na1, _, na2) = r1 /= Range.DISJUNCT && r2 /= Range.DISJUNCT
			where
				r1 = Range.relation n1 [msrc]
				r2 = Range.relation n2 [mdst]
				n1 = getNetAtomIpRangeSmart devs0 na1
				n2 = getNetAtomIpRangeSmart devs0 na2
		pathsmatchWithDev = filter pathWithDevQ pathsmatch
		pathWithDevQ (_, devs, _) = dev `elem` devs

validateDev :: Net.Device -> Net.Device
validateDev dev = if (or results) then (error "Intersection of interfaces found") else dev
	where
		itfs = filter (not . interfaceIsInternet) (getInterfaces dev)
		itf2range = head . getNetAtomIpRanges . getNetworkOfInterface
		itfranges = if length itfs == length (List.nub itfs)
			then [ (itf1, itf2) | itf1 <- itfs, itf2 <- itfs, itf1 < itf2 ]
			else (error $ Doc.showrepr $ Doc.repr "Repeated interface in" Doc.<+> Doc.name dev)
		results = map ( \ (i1, i2) -> checki i1 i2 ) itfranges
		checki i1 i2 = if Range.intersectionOfRange r1 r2 == Nothing
			then False
			else (error $ Doc.showrepr $ Doc.repr "Intersection of interfaces:" Doc.<+> Doc.name dev Doc.<+> Doc.name (i1, i2))
			where
				r1 = itf2range i1
				r2 = itf2range i2

validate :: [Net.Device] -> [Net.Device]
validate devs = map validateDev devs

------------------------------------------------------------------------------

