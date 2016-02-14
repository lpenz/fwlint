
module Match where

#ifndef PROFILE
import Test.QuickCheck
#endif

import Monad
import qualified Range
import qualified Ip
import qualified Parse
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Doc
import qualified Maybe

------------------------------------------------------------------------------

data Match = Match {
	maProt::Range.Range Ip.Protocol,
	maAsrc::Range.Range Ip.Addr,
	maPsrc::Range.Range Ip.Port,
	maAdst::Range.Range Ip.Addr,
	maPdst::Range.Range Ip.Port
	} deriving (Show, Read, Eq, Ord)

instance Doc.Able Match where
	repr m = Doc.struhcat $ Maybe.catMaybes [
		docProto $ maProt m,
		docSocket "src" (maAsrc m, maPsrc m),
		docSocket "dst" (maAdst m, maPdst m)
		]
	name m = Doc.sep $ Maybe.catMaybes [
		docProto $ maProt m,
		docSocket "src" (maAsrc m, maPsrc m),
		docSocket "dst" (maAdst m, maPdst m)
		]

docProto :: (Range.Range Ip.Protocol) -> Maybe Doc.Doc
docProto proto = if proto == protocolRangeTotal then Nothing else Just (Doc.repr proto)

docSocket :: String -> (Range.Range Ip.Addr, Range.Range Ip.Port) -> Maybe Doc.Doc
docSocket str (a, p) =
	if (a == ipRangeTotal && p == portRangeTotal) then Nothing else Just (Doc.repr str Doc.<+> Doc.hcat [
		(if a /= ipRangeTotal   then Doc.repr a else Doc.empty),
		(if p /= portRangeTotal then (Doc.repr ':' Doc.<> Doc.repr p) else Doc.empty)])

instance Range.Rangeable Match where
	toListOfRanges = Range.toListOfRanges . maToTuple
	fromListOfRanges = tupleToMatch . Range.fromListOfRanges

maToTuple::Match -> (Range.Range Ip.Protocol, Range.Range Ip.Addr, Range.Range Ip.Port, Range.Range Ip.Addr, Range.Range Ip.Port)
maToTuple (Match proto asrc psrc adst pdst) = (
	proto,
	asrc, psrc,
	adst, pdst)

maGetSrc :: Match -> (Range.Range Ip.Addr, Range.Range Ip.Port)
maGetSrc m = (maAsrc m, maPsrc m)

maGetDst :: Match -> (Range.Range Ip.Addr, Range.Range Ip.Port)
maGetDst m = (maAdst m, maPdst m)

maIntersectSrcDst :: (Range.Range Ip.Addr, Range.Range Ip.Addr) -> Match -> Maybe Match
maIntersectSrcDst (src, dst) (Match proto asrc psrc adst pdst) = case (isrc, idst) of
	(Nothing, _) -> Nothing
	(_, Nothing) -> Nothing
	(Just s, Just d) -> Just $ Match proto s psrc d pdst
	where
		isrc = Range.intersection asrc src
		idst = Range.intersection adst dst

tupleToMatch::(Range.Range Ip.Protocol, Range.Range Ip.Addr, Range.Range Ip.Port, Range.Range Ip.Addr, Range.Range Ip.Port) -> Match
tupleToMatch (proto, asrc, psrc, adst, pdst) = Match proto asrc psrc adst pdst

maMatches :: Match -> (Ip.Addr, Ip.Addr) -> Bool
maMatches ma (src, dst) = Range.matchRange msrc src && Range.matchRange mdst dst
	where
		(_, msrc, _, mdst, _) = maToTuple ma

ipRangeTotal :: Range.Range Ip.Addr
ipRangeTotal = Range.Range (Ip.makeAddr 0 0 0 0) (Ip.makeAddr 255 255 255 255)

portRangeTotal :: Range.Range Ip.Port
portRangeTotal = Range.Range (Ip.Port 0) (Ip.Port 65535)

protocolRangeTotal :: Range.Range Ip.Protocol
protocolRangeTotal = Range.Range (Ip.Protocol 0) (Ip.Protocol 2)

maIps :: Ip.Addr -> Ip.Addr -> Match
maIps src dst = Match {
	maProt = protocolRangeTotal,
	maAsrc = Range.Range src src,
	maPsrc = portRangeTotal,
	maAdst = Range.Range dst dst,
	maPdst = portRangeTotal
	}

maGetIps :: Match -> (Range.Range Ip.Addr, Range.Range Ip.Addr)
maGetIps (Match _ src _ dst _) = (src, dst)

defaultMA :: Match
defaultMA = Match {
	maProt = protocolRangeTotal,
	maAsrc = ipRangeTotal,
	maPsrc = portRangeTotal,
	maAdst = ipRangeTotal,
	maPdst = portRangeTotal
	}

socketRangeGlobal :: (Range.Range Ip.Addr, Range.Range Ip.Port)
socketRangeGlobal = (Range.Range (Ip.makeAddr 0 0 0 0) (Ip.makeAddr 255 255 255 255), Range.Range (Ip.buildPort 0) (Ip.buildPort 65535))

#ifndef PROFILE
instance Arbitrary Match where
	arbitrary =
		do
			let lrange = liftM2 Range.Range
			let r a = Range.Range a a
			pr <- oneof $ map return [r Ip.icmp, r Ip.tcp, r Ip.udp]
			as <- lrange arbitrary arbitrary
			ps <- lrange arbitrary arbitrary
			ad <- lrange arbitrary arbitrary
			pd <- lrange arbitrary arbitrary
			return $ Match pr as ps ad pd
#endif

------------------------------------------------------------------------------

parseIpPort :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
parseIpPort = Parse.lexeme $ do
	Parsec.try parseIpRuleWithPort
	Parsec.<|> (Parsec.try parseIpRuleWithOutPort)
	Parsec.<|> parseIpRuleOnlyPort
	where
		parseIpRuleWithPort :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
		parseIpRuleWithPort = do
			ip <- Parse.parse
			Parsec.char ':'
			ports <- Parse.parse
			return $ (ip, ports)
		parseIpRuleWithOutPort :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
		parseIpRuleWithOutPort = do
			ip <- Parse.parse
			return $ (ip, Range.Range (Ip.Port 0) (Ip.Port 65535))
		parseIpRuleOnlyPort :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
		parseIpRuleOnlyPort = do
			Parsec.char ':'
			ports <- Parse.parse
			return $ (Ip.getIpNetRange (Ip.makeAddr 0 0 0 0) 0, ports)

parseSocketRange :: Parse.Parser ((Range.Range Ip.Addr, Range.Range Ip.Port), (Range.Range Ip.Addr, Range.Range Ip.Port))
parseSocketRange = Parse.lexeme $ do
	Parsec.try parseSrcDst
	Parsec.<|> (Parsec.try parseDstSrc)
	where
		parseSrc :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
		parseSrc = do
			Parse.reserved "src"
			src <- parseIpPort
			Parse.comma
			return $ src
		parseDst :: Parse.Parser (Range.Range Ip.Addr, Range.Range Ip.Port)
		parseDst = do
			Parse.reserved "dst"
			dst <- parseIpPort
			Parse.comma
			return $ dst
		parseSrcDst :: Parse.Parser ((Range.Range Ip.Addr, Range.Range Ip.Port), (Range.Range Ip.Addr, Range.Range Ip.Port))
		parseSrcDst = do
			src <- Parsec.option socketRangeGlobal parseSrc
			dst <- Parsec.option socketRangeGlobal parseDst
			return $ (src, dst)
		parseDstSrc :: Parse.Parser ((Range.Range Ip.Addr, Range.Range Ip.Port), (Range.Range Ip.Addr, Range.Range Ip.Port))
		parseDstSrc = do
			dst <- Parsec.option socketRangeGlobal parseDst
			src <- Parsec.option socketRangeGlobal parseSrc
			return $ (src, dst)

instance Parse.Parseable Match where
	parseImpl = Parse.lexeme $ do
		proto <- Parsec.option protocolRangeTotal Parse.parse
		(src, dst) <- Parsec.option (socketRangeGlobal, socketRangeGlobal) parseSocketRange
		return $ Match proto (fst src) (snd src) (fst dst) (snd dst)

