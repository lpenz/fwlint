
module Ip where
import Data.Bits

#ifndef PROFILE
import Test.QuickCheck
#endif

import Monad
import Text.Printf
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Parse
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Doc
import qualified Range
import IntegerIsomorph

------------------------------------------------------------------------------

-- IP:

data Addr = 
	Addr Integer
	deriving (Eq, Show, Read, Ord)

instance IntegerIsomorph Addr where
	toIntegerSpace = convertIpToNum
	fromIntegerSpace = convertNumToIp

instance Doc.Able Addr where
	repr ip = Doc.text $ printf "%d.%d.%d.%d" ip1 ip2 ip3 ip4
		where
			(ip1, ip2, ip3, ip4) = getIpComponents ip

instance Num Addr where
	(+) (Addr ip1) (Addr ip2) = Addr $ ip2 + ip1
	(-) (Addr ip1) (Addr ip2) = Addr $ ip1 - ip2
	(*) _ _  = error "* not implemented"
	--negate _ = error "Not implemented"
	abs _    = error "abs not implemented"
	signum _ = error "signum not implemented"
	fromInteger ip = Addr ip

instance Parse.Parseable Addr where
	parseImpl = do
		ip1 <- parseIpComp Parsec.<?> "valid IP address"
		Parsec.char '.' Parsec.<?> "valid IP address"
		ip2 <- parseIpComp Parsec.<?> "valid IP address"
		Parsec.char '.' Parsec.<?> "valid IP address"
		ip3 <- parseIpComp Parsec.<?> "valid IP address"
		Parsec.char '.' Parsec.<?> "valid IP address"
		ip4 <- parseIpComp Parsec.<?> "valid IP address"
		return (makeAddr ip1 ip2 ip3 ip4)
		where
			parseIpComp325 :: Parse.Parser Int
			parseIpComp325 = do
				Parsec.char '2'
				Parsec.char '5'
				c <- Parsec.oneOf "012345"
				return $ 250 + (read [c])
			parseIpComp324 :: Parse.Parser Int
			parseIpComp324 = do
				Parsec.char '2'
				b <- Parsec.oneOf "01234"
				c <- parseIpComp1
				return $ 200 + (read [b])*10 + c
			parseIpComp31 :: Parse.Parser Int
			parseIpComp31 = do
				Parsec.char '1'
				b <- parseIpComp2
				return $ 100 + b
			parseIpComp2 :: Parse.Parser Int
			parseIpComp2 = do
				b <- Parsec.digit
				c <- parseIpComp1
				return $ (read [b])*10 + c
			parseIpComp1 :: Parse.Parser Int
			parseIpComp1 = do
				c <- Parsec.digit
				return $ (read [c])
			parseIpComp = do
				Parsec.try parseIpComp325
				Parsec.<|> Parsec.try parseIpComp324
				Parsec.<|> Parsec.try parseIpComp31
				Parsec.<|> Parsec.try parseIpComp2
				Parsec.<|> parseIpComp1

#ifndef PROFILE
instance Arbitrary Addr where
	arbitrary =
		do
			ip1  <- choose (0, 255)
			ip2  <- choose (0, 255)
			ip3  <- choose (0, 255)
			ip4  <- choose (0, 255)
			return $ makeAddr ip1 ip2 ip3 ip4
	coarbitrary = undefined
#endif

proxyAddr::Addr
proxyAddr = error "Do not use"

getIpComponents :: Addr -> (Int, Int, Int, Int)
getIpComponents (Addr ip) = (ip1, ip2, ip3, ip4)
	where
		ip1 = fromIntegral $ (shiftR ip 24) .&. 0xff
		ip2 = fromIntegral $ (shiftR ip 16) .&. 0xff
		ip3 = fromIntegral $ (shiftR ip 8) .&. 0xff
		ip4 = fromIntegral $ (shiftR ip 0) .&. 0xff

convertIpToNum::Addr -> Integer
convertIpToNum (Addr ip) = ip

maxIpNum :: Integer
maxIpNum = 4294967295

convertNumToIp::Integer -> Addr
convertNumToIp ip | ip < 0 = Addr 0
convertNumToIp ip | ip > maxIpNum = Addr maxIpNum
convertNumToIp ip = Addr ip

addrRangeTotal :: Range.Range Addr
addrRangeTotal = Range.Range (convertNumToIp 0) (convertNumToIp maxIpNum)

makeAddr :: Int -> Int -> Int -> Int -> Addr
makeAddr ip1 ip2 ip3 ip4 =
	if ipnum > maxIpNum || ipnum < 0 then
		error (printf "Invalid IP address %d.%d.%d.%d" ip1 ip2 ip3 ip4)
	else
		Addr $ ipnum
	where
		ipl1 = fromIntegral ip1
		ipl2 = fromIntegral ip2
		ipl3 = fromIntegral ip3
		ipl4 = fromIntegral ip4
		ipnum = (shiftL ipl1 24) + (shiftL ipl2 16) + (shiftL ipl3 8) + ipl4

getRangeIpImportantIp::Range.Range Addr -> [Addr]
getRangeIpImportantIp a = Range.importantElements [a]

------------------------------------------------------------------------------

-- Ip Range

instance Doc.Able (Range.Range Addr) where
	repr r = case getRangeIpNet r of
		Nothing -> Doc.hcat [ Doc.repr $ Range.iniOfRange r, Doc.repr '-', Doc.repr $ Range.fimOfRange r ]
		Just (ip, pl) -> Doc.hcat [ Doc.repr ip, Doc.repr '/', Doc.repr pl ]

instance Parse.Parseable (Range.Range Addr) where
	parseImpl = do
		Parsec.try parseIpNet3
		Parsec.<|> Parsec.try parseIpNet2
		Parsec.<|> Parsec.try parseIpNet1
		Parsec.<|> Parsec.try parseIpRange
		Parsec.<|> Parsec.try parseIpNet0
		where
			parseIpNet3 :: Parse.Parser (Range.Range Addr)
			parseIpNet3 = do
				ip <- Parse.parse
				Parsec.string "/3"
				a <- Parsec.oneOf "012"
				return $ getIpNetRange ip (30 + (read [a]))
			parseIpNet2 :: Parse.Parser (Range.Range Addr)
			parseIpNet2 = do
				ip <- Parse.parse
				Parsec.char '/'
				a <- Parsec.oneOf "012"
				b <- Parsec.digit
				return $ getIpNetRange ip ((read [a] * 10) + (read [b]))
			parseIpNet1 :: Parse.Parser (Range.Range Addr)
			parseIpNet1 = do
				ip <- Parse.parse
				Parsec.char '/'
				a <- Parsec.digit
				return $ getIpNetRange ip (read [a])
			parseIpNet0 :: Parse.Parser (Range.Range Addr)
			parseIpNet0 = do
				ip <- Parse.parse
				return $ getIpNetRange ip 32
			parseIpRange = do
				Parsec.try parseTwo
				Parsec.<|> parseOne
				where
					parseTwo = do
						vini <- Parse.parse
						Parsec.char '-'
						vfim <- Parse.parse
						return $ Range.Range vini vfim
					parseOne = do
						val <- Parse.parse
						return $ Range.Range val val

------------------------------------------------------------------------------

-- Ip net:

type IpNet = (Addr, Int)

instance Doc.Able (Addr, Int) where
	repr (ip, pl) = Doc.hcat [ Doc.repr ip , Doc.repr "/", Doc.repr pl ]

instance Parse.Parseable (Addr, Int) where
	parseImpl = Parse.lexeme $ do
		ip <- Parse.parse
		Parse.symbol "/"
		pl <- Monad.liftM fromInteger $ Parse.parse
		return $ if pl > 32 || pl < 0 then error "Invalid prefix length" else (ip, pl)

getIpNetMinIp :: Addr -> Int -> Addr
getIpNetMinIp (Addr ipnum) pl = Addr ipnummin
	where
		plenint = fromIntegral $ 32 - pl
		ipnummin = (shiftL (shiftR ipnum plenint) plenint)

getIpNetMaxIp :: Addr -> Int -> Addr
getIpNetMaxIp (Addr ipnum) pl = Addr ipnummax
	where
		plenint = fromIntegral $ pl
		ipnummax = (shiftR ((shiftL 0xffffffff plenint) .&. 0xffffffff) plenint) .|. ipnum

getIpNetRange :: Addr -> Int -> Range.Range Addr
getIpNetRange ipn pl = Range.Range (getIpNetMinIp ipn pl) (getIpNetMaxIp ipn pl)

getRangeIpNet :: Range.Range Addr -> Maybe (Addr, Int)
getRangeIpNet ipr = List.lookup (ip1, ip2) other
	where
		ip1 = head $ Range.ini ipr
		ip2 = head $ Range.fim ipr
		list = List.map ( \ i -> (ip1, i)) [0..32]
		other :: [((Addr, Addr), (Addr, Int))]
		other = List.map ( \ (ip, pl) -> ((getIpNetMinIp ip pl, getIpNetMaxIp ip pl), (ip, pl))) list

fixIpNet :: Addr -> Int -> IpNet
fixIpNet ip pl = (getIpNetMinIp ip pl, pl)

------------------------------------------------------------------------------

-- IP ports:

data Port = Port Integer deriving (Show, Read, Ord, Eq)

instance Doc.Able Port where
	repr (Port p) = Doc.repr p

instance IntegerIsomorph Port where
	toIntegerSpace (Port p) = p
	fromIntegerSpace = Port

instance Parse.Parseable Port where
	parseImpl = do
		p <- Parsec.many1 Parsec.digit
		return $ buildPort $ read p

#ifndef PROFILE
instance Arbitrary Port where
	arbitrary = do
		p <- choose (0, 65535)
		return $ Port p
	coarbitrary = undefined
#endif

validPort :: Port -> Bool
validPort (Port p) = p >= 0 && p <= 65535

buildPort :: Integer -> Port
buildPort par = if validPort p then p else error ("invalid port " ++ show p)
	where p = Port par

------------------------------------------------------------------------------

-- IP Protocol:

data Protocol = Protocol Integer deriving (Show, Read, Ord, Eq)

instance IntegerIsomorph Protocol where
	toIntegerSpace (Protocol p) = p
	fromIntegerSpace = buildProtocol

icmp :: Protocol
icmp = Protocol 0

udp :: Protocol
udp = Protocol 1

tcp :: Protocol
tcp = Protocol 2

instance Doc.Able Protocol where
	repr (Protocol p) = Doc.repr $ Maybe.fromJust $ lookup p [(0, "ICMP"), (1, "UDP"), (2, "TCP")]

#ifndef PROFILE
instance Arbitrary Protocol where
	arbitrary = do
		p <- choose (0, 2)
		return $ Protocol p
	coarbitrary = undefined
#endif

instance Parse.Parseable Protocol where
	parseImpl = do
		Parsec.try parseICMP Parsec.<|> Parsec.try parseUDP Parsec.<|> parseTCP
		where
			parseICMP = do
				Parse.reserved "ICMP"
				return icmp
			parseUDP = do
				Parse.reserved "UDP"
				return udp
			parseTCP = do
				Parse.reserved "TCP"
				return tcp

validProtocol :: Protocol -> Bool
validProtocol (Protocol p) = p >= 0 && p <= 2

buildProtocol :: Integer -> Protocol
buildProtocol par = if validProtocol p then p else error ("invalid protocol " ++ show p)
	where p = Protocol par


------------------------------------------------------------------------------

-- Tests

#ifndef PROFILE

prop_Addr_IpFromNumIsNumFromIpInverse :: Addr -> Bool
prop_Addr_IpFromNumIsNumFromIpInverse ip = ip == (convertNumToIp $ convertIpToNum ip)

prop_IpNet_MinIpLessThanMaxIp :: Addr -> Int -> Property
prop_IpNet_MinIpLessThanMaxIp ip pl = pl >= 0 && pl <= 32 ==> convertIpToNum(getIpNetMinIp ip pl) <= convertIpToNum(getIpNetMaxIp ip pl)

prop_getIpNetOfInverse :: Addr -> Int -> Property
prop_getIpNetOfInverse ip pl = pl >= 0 && pl <= 32 ==> getRangeIpNet (Range.Range ip1 ip2) == Just (ip1, pl)
	where
		ip1 = getIpNetMinIp ip pl
		ip2 = getIpNetMaxIp ip pl

prop_IpNet_LenImportantIp :: Addr -> Int -> Property
prop_IpNet_LenImportantIp ip pl = pl >= 0 && pl <= 32 ==> case pl of
	32 -> (length . Range.importantElementsOfRange) (getIpNetRange ip 32) == 3
	_ -> (length . Range.importantElementsOfRange) (getIpNetRange ip pl) == 4

assert_maxIpNum :: Bool
assert_maxIpNum = maxIpNum > 0

#endif

