
module Range where

#ifndef PROFILE
import Test.QuickCheck
import Random
import qualified Tuple
#endif

import qualified Data.Maybe as Maybe
import List
import Monad
import Doc
import qualified Parse
import qualified Text.ParserCombinators.Parsec as Parsec
import IntegerIsomorph

data Range a = Range a a
	deriving (Show, Read, Ord)

data RangeRelation = EQUAL | CONTAINS | CONTAINED | CORELATED | DISJUNCT deriving (Show, Read, Eq)

instance Doc.Able RangeRelation

instance Functor Range where
	fmap f (Range a b) = Range (f a) (f b)

instance (Doc.Able a, Ord a) => Doc.Able (Range a) where
	repr a = if i == f then repr i else (repr i <> repr '-' <> repr f)
		where
			i = iniOfRange a
			f = fimOfRange a

instance Ord a => Eq (Range a) where
	(==) a b = iniOfRange a == iniOfRange b && fimOfRange a == fimOfRange b

iniOfRange :: Ord a => Range a -> a
iniOfRange (Range a b) = min a b

fimOfRange :: Ord a => Range a -> a
fimOfRange (Range a b) = max a b

isInside :: Ord a => Range a -> Range a -> Bool
isInside big small = (iniOfRange big <= iniOfRange small) && (fimOfRange big >= fimOfRange small) && big /= small

isAnyInside :: Ord a => Range a -> Range a -> Bool
isAnyInside a b = (isInside a b) || (isInside b a)

matchRange :: (Ord a) => Range a -> a -> Bool
matchRange r i = (iniOfRange r) <= i && i <= (fimOfRange r)

sizeOfRange :: (Num a, Ord a) => Range a -> a
sizeOfRange r = fimOfRange r - iniOfRange r + 1

relationOfRange :: Ord a => Range a -> Range a -> RangeRelation
relationOfRange r1 r2 | r1 == r2 = EQUAL
relationOfRange r1 r2 | isInside r1 r2 = CONTAINS
relationOfRange r1 r2 | isInside r2 r1 = CONTAINED
relationOfRange r1 r2 | intersectionOfRange r1 r2 /= Nothing = CORELATED
relationOfRange _  _  = DISJUNCT

intersectionOfRange :: Ord a => Range a -> Range a -> Maybe (Range a)
intersectionOfRange a b
	| fimOfRange a < iniOfRange b = Nothing
	| fimOfRange b < iniOfRange a = Nothing
	| otherwise =
		Just (Range i f) where
			i = max (iniOfRange a) (iniOfRange b)
			f = min (fimOfRange a) (fimOfRange b)

differenceOfRange :: (Num a, Ord a) => Range a -> Range a -> [Range a]
differenceOfRange a b
	| intersectionOfRange a b == Nothing = [a]
	| iniOfRange a < iniOfRange b = (Range (iniOfRange a) (iniOfRange b - 1)):(differenceOfRange (Range (iniOfRange b) (fimOfRange a)) b)
	| fimOfRange a > fimOfRange b = (Range (fimOfRange b + 1) (fimOfRange a)):(differenceOfRange (Range (iniOfRange a) (fimOfRange b)) b)
	| otherwise = []

importantElementsOfRange :: (Num a, Ord a) => Range a -> [a]
importantElementsOfRange r = if i /= f then [i - 1, i, f, f + 1] else [i - 1, i, f + 1]
	where
		i = iniOfRange r
		f = fimOfRange r

------------------------------------------------------------------------------

-- Lists:

matchRanges :: (Ord a) => [Range a] -> [a] -> Bool
matchRanges (r:rs) (i:is) = matchRange r i && matchRanges rs is
matchRanges [] [] = True
matchRanges _ _ = error "matchRanges: error in dimension"

iniOfRanges :: Ord a => [Range a] -> [a]
iniOfRanges = map iniOfRange

fimOfRanges :: Ord a => [Range a] -> [a]
fimOfRanges = map fimOfRange

sizeOfRanges :: (Num a, Ord a) => [Range a] -> a
sizeOfRanges r | length r == 0 = 0
sizeOfRanges r = foldr (*) 1 $ map sizeOfRange r

relationOfRanges :: Ord a => [Range a] -> [Range a] -> RangeRelation
relationOfRanges r1 r2 | r1 == r2 = EQUAL
relationOfRanges r1 r2 | intersectionOfRanges r1 r2 == Just r2 = CONTAINS
relationOfRanges r1 r2 | intersectionOfRanges r1 r2 == Just r1 = CONTAINED
relationOfRanges r1 r2 | intersectionOfRanges r1 r2 /= Nothing = CORELATED
relationOfRanges _  _  = DISJUNCT

intersectionOfRanges :: Ord a => [Range a] -> [Range a] -> Maybe [Range a]
intersectionOfRanges r s
	| elem Nothing $ zipWith intersectionOfRange r s = Nothing
	| otherwise = Just $ map Maybe.fromJust $ zipWith (intersectionOfRange) r s

differenceOfRanges :: (Num a, Ord a) => [Range a] -> [Range a] -> [[Range a]]
differenceOfRanges rt@(r:rs) st@(s:ss)
	| intersectionOfRanges rt st == Nothing = [rt]
	| intersectionOfRange r s == Just r = [ r:d | d <- differenceOfRanges rs ss ]
	| otherwise = [ d:rs | d <- differenceOfRange r s ] ++ [(Maybe.fromJust $ intersectionOfRange r s):d | d <- differenceOfRanges rs ss ]
differenceOfRanges [] [] = []
differenceOfRanges _ _ = error "differenceOfRanges: Error in dimension"

importantElementsOfRanges :: (Num a, Ord a) => [Range a] -> [a]
importantElementsOfRanges rs = (nub . sort . concat) (map importantElementsOfRange rs)

------------------------------------------------------------------------------

-- For integer isomorphs:

toRangeOfInteger :: IntegerIsomorph a => Range a -> Range Integer
toRangeOfInteger ra = Range (toIntegerSpace $ iniOfRange ra) (toIntegerSpace $ fimOfRange ra)

fromRangeOfInteger :: IntegerIsomorph a => Range Integer -> Range a
fromRangeOfInteger ri = Range (fromIntegerSpace $ iniOfRange ri) (fromIntegerSpace $ fimOfRange ri)

toRangesOfIntegers :: IntegerIsomorph a => [Range a] -> [Range Integer]
toRangesOfIntegers = map toRangeOfInteger

fromRangesOfIntegers :: IntegerIsomorph a => [Range Integer] -> [Range a]
fromRangesOfIntegers = map fromRangeOfInteger

------------------------------------------------------------------------------

-- Class:

class Rangeable a where
	toListOfRanges :: a -> [Range Integer]
	fromListOfRanges :: [Range Integer] -> a
	intersection :: (Rangeable a) => a -> a -> Maybe a
	intersection r s = case intersectionOfRanges (toListOfRanges r) (toListOfRanges s) of
		Nothing -> Nothing
		Just a -> Just $ fromListOfRanges a
	difference :: (Rangeable a) => a -> a -> [a]
	difference r s = map fromListOfRanges (differenceOfRanges (toListOfRanges r) (toListOfRanges s))


instance (IntegerIsomorph a) => Rangeable ([Range a]) where
	toListOfRanges = toRangesOfIntegers
	fromListOfRanges = fromRangesOfIntegers

instance (IntegerIsomorph a, IntegerIsomorph b) => Rangeable (Range a, Range b) where
	toListOfRanges r = [toRangeOfInteger $ fst r, toRangeOfInteger $ snd r]
	fromListOfRanges (a:b:[]) = (fromRangeOfInteger a, fromRangeOfInteger b)
	fromListOfRanges _ = error "Invalid dimension, should be 2"

instance (IntegerIsomorph a, IntegerIsomorph b, IntegerIsomorph c, IntegerIsomorph d) =>
	Rangeable (Range a, Range b, Range c, Range d) where
	toListOfRanges (r1, r2, r3, r4) = [toRangeOfInteger r1, toRangeOfInteger r2, toRangeOfInteger r3, toRangeOfInteger r4]
	fromListOfRanges (a:b:c:d:[]) = (fromRangeOfInteger a, fromRangeOfInteger b, fromRangeOfInteger c, fromRangeOfInteger d)
	fromListOfRanges _ = error "Invalid dimension, should be 4"

instance (IntegerIsomorph a, IntegerIsomorph b, IntegerIsomorph c, IntegerIsomorph d, IntegerIsomorph e) =>
	Rangeable (Range a, Range b, Range c, Range d, Range e) where
	toListOfRanges (r1, r2, r3, r4, r5) = [toRangeOfInteger r1, toRangeOfInteger r2, toRangeOfInteger r3, toRangeOfInteger r4, toRangeOfInteger r5]
	fromListOfRanges (a:b:c:d:e:[]) = (fromRangeOfInteger a, fromRangeOfInteger b, fromRangeOfInteger c, fromRangeOfInteger d, fromRangeOfInteger e)
	fromListOfRanges _ = error "Invalid dimension, should be 5"

instance (IntegerIsomorph a) => Rangeable (Range a) where
	toListOfRanges (Range i f) = [Range (toIntegerSpace i) (toIntegerSpace f)]
	fromListOfRanges (e:[]) = fmap fromIntegerSpace e
	fromListOfRanges _ = error "incompatible range dimension"

ini :: (Rangeable a, IntegerIsomorph b) => a -> [b]
ini = (map fromIntegerSpace) . iniOfRanges . toListOfRanges

fim :: (Rangeable a, IntegerIsomorph b) => a -> [b]
fim = (map fromIntegerSpace) . fimOfRanges . toListOfRanges

match :: (Rangeable a, IntegerIsomorph b) => a -> [b] -> Bool
match rs vs = matchRanges ranges values
	where
		ranges = toListOfRanges rs
		values = map toIntegerSpace vs

importantElements :: (Rangeable a, IntegerIsomorph b) => a -> [b]
importantElements = (map fromIntegerSpace) . importantElementsOfRanges . toListOfRanges

size :: (Rangeable a) => a -> Integer
size = sizeOfRanges . toListOfRanges

relation :: (Rangeable a) => a -> a -> RangeRelation
relation r s = relationOfRanges (toListOfRanges r) (toListOfRanges s)

------------------------------------------------------------------------------

instance Parse.Parseable a => Parse.Parseable (Range a) where
	parseImpl = do
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

-- Tests:

#ifndef PROFILE
instance (Arbitrary a, Random a, Num a) => Arbitrary (Range a) where
	arbitrary =
		do
			i  <- choose (0, 255)
			f  <- choose (0, 255)
			return $ Range i f
	coarbitrary = undefined

prop_Range_OfEqual :: Range Int -> Bool
prop_Range_OfEqual r = intersectionOfRange r r == Just r && isInside r r == False && isAnyInside r r == False

prop_Range_IniFim :: Range Int -> Bool
prop_Range_IniFim r = iniOfRange r <= fimOfRange r

prop_Range_isInside :: Range Int -> Range Int -> Property
prop_Range_isInside r s = isInside r s ==> sizeOfRange r > sizeOfRange s && isAnyInside r s && Maybe.fromJust (intersectionOfRange r s) == s

prop_Range_Relations :: [Range Int] -> [Range Int] -> Bool
prop_Range_Relations r s =
	case relationOfRanges r s of
		CONTAINS  -> relationOfRanges s r == CONTAINED
		CONTAINED -> relationOfRanges s r == CONTAINS
		CORELATED -> intersectionOfRanges r s /= Nothing
		EQUAL -> intersectionOfRanges r s == Just r
		DISJUNCT -> intersectionOfRanges r s == Nothing

prop_Range_differenceIntersectionSizes :: Range Int -> Range Int -> Bool
prop_Range_differenceIntersectionSizes r s 
	= sizeOfRange r + sizeOfRange s - sizeofintersect ==
	sizeofintersect + (foldr1 (+) $ map sizeOfRange $ differenceOfRange r s ++ differenceOfRange s r)
	where
		sizeofintersect = case intersectionOfRange r s of
			Nothing -> 0
			Just i  -> sizeOfRange i

prop_Range_differenceIntersectionSizeMultiDimensions :: [Range Int] -> [Range Int] -> Property
prop_Range_differenceIntersectionSizeMultiDimensions r s 
	= length r == length s ==>
	sizeOfRanges r + sizeOfRanges s - sizeofintersect ==
	sizeofintersect + (foldr (+) 0 $ map sizeOfRanges $ differenceOfRanges r s ++ differenceOfRanges s r)
	where
		sizeofintersect = case intersectionOfRanges r s of
			Nothing -> 0
			Just i  -> sizeOfRanges i

prop_Range_differenceWhenNoIntersection :: [Range Int] -> [Range Int] -> Property
prop_Range_differenceWhenNoIntersection r s 
	= intersectionOfRanges r s == Nothing ==> differenceOfRanges r s == [r]

prop_FuncsEqualForRangeAndRanges :: Range Integer -> Range Integer -> Bool
prop_FuncsEqualForRangeAndRanges r s = True
	&& iniOfRange r == head (iniOfRanges [r])
	&& fimOfRange r == head (fimOfRanges [r])
	&& sizeOfRange r == sizeOfRanges [r]
	&& relationOfRange r s == relationOfRanges [r] [s]
	&& intersectionOfRange r s == case intersectionOfRanges [r] [s] of
		Nothing -> Nothing
		Just l -> Just $ head l
	&& differenceOfRange r s == case differenceOfRanges [r] [s] of
		[] -> []
		a -> map head a
	&& importantElementsOfRange r == importantElementsOfRanges [r]

	&& (ini r ::[Integer]) == ini [r]
	&& (fim r ::[Integer]) == fim [r]
	&& size r == size [r]
	&& relation r s == relation [r] [s]
	&& intersection r s == fmap head (intersection [r] [s])
	&& difference r s == fmap head (difference [r] [s])
	&& (importantElements r ::[Integer]) == importantElements [r]

prop_FuncsEqualForTRangeAndLRanges :: (Range Integer, Range Integer) -> (Range Integer, Range Integer) -> Integer -> Bool
prop_FuncsEqualForTRangeAndLRanges tr ts el = True
	&& (ini lr :: [Integer]) == ini tr
	&& (fim lr :: [Integer]) == fim tr
	&& size lr == size tr
	&& relation tr ts == relation lr ls
	&& fmap Tuple.toList (intersection tr ts) == intersection lr ls
	&& fmap Tuple.toList (difference tr ts) == difference lr ls
	&& (importantElements tr ::[Integer]) == importantElements lr
	&& match tr [el, el] == match lr [el, el]
	where
		lr = [fst tr, snd tr]
		ls = [fst ts, snd ts]

prop_DisjunctRangesDisjunct :: [Range Integer] -> Property
prop_DisjunctRangesDisjunct ranges = not (aredis ranges) ==> aredis $ (map snd) $ buildDisjunct ranges
	where
		aredis :: [Range Integer] -> Bool
		aredis rs = and $ map ((==) DISJUNCT) [relation a b | a <- rs, b <- rs, a /= b]

#endif

------------------------------------------------------------------------------

--  Inner label region (inner) (outer) | Leaf [regions]
data Tree a = Inner a (Tree a) (Tree a) | Leaf [a]
	deriving (Show, Eq)

instance Doc.Able a => Doc.Able (Tree a) where
	repr (Inner l i o) = (Doc.hcat [ Doc.repr "{Inner ", Doc.repr $ l ]) Doc.$$ (Doc.nestup $ Doc.repr "Ins: " Doc.<> Doc.repr i) Doc.$$ (Doc.nestup $ Doc.repr "Out: " Doc.<> Doc.repr o) Doc.<> Doc.repr "}"
	repr (Leaf es) = Doc.hsep [ Doc.text "{Leaf", Doc.repr es, Doc.text "}" ]

buildRangeTree :: (Eq a, Rangeable a) => [a] -> Tree a -> Tree a
buildRangeTree [] toptree = toptree
buildRangeTree toplist toptree = foldr addRealNode toptree toplist
	where
		--addRealNode :: Rangeable a => a -> Tree a -> Tree a
		addRealNode new tree = addNode (new, [new]) tree
		addNode (_, []) node = node
		addNode (nlab, nregs) (Inner l i o) = rv
			where
				nodeins = addNode (nlab, Maybe.mapMaybe (Range.intersection l) nregs) i
				nodeout = addNode (nlab, concatMap ( \ r -> Range.difference r l) nregs) o
				rv = seq nodeins $ Inner l nodeins nodeout
		addNode (nlab, nregs) (Leaf regs) = rv
			where
				leafins = Leaf $ List.nub $ Maybe.mapMaybe ( \ (ro, rn) -> Range.intersection ro rn) allregs
				leafout = Leaf $ List.nub $ concatMap ( \ (ro, rn) -> Range.difference ro rn) allregs
				allregs = [(ro, rn) | ro <- regs, rn <- nregs ]
				rv = seq leafins $ Inner nlab leafins leafout

buildEquivalentWithRegion :: (Eq a, Rangeable a) => Tree a -> [([a], [a])]
buildEquivalentWithRegion t = ( (map ( \ (r, e) -> (r, reverse e))) . (filter (not . null . snd)) ) (buildEquivalentInternalWithRegion t)

buildEquivalentInternalWithRegion :: (Eq a, Rangeable a) => Tree a -> [([a], [a])]
buildEquivalentInternalWithRegion (Inner n i o) = equivn ++ equivo
	where
		equivn = map ( \ (r, e) -> (r, n:e)) equivi
		equivi = buildEquivalentInternalWithRegion i
		equivo = buildEquivalentInternalWithRegion o
buildEquivalentInternalWithRegion (Leaf []) = []
buildEquivalentInternalWithRegion (Leaf r) = [(r, [])]

buildEquivalent :: (Eq a, Rangeable a) => Tree a -> [[a]]
buildEquivalent t = nub $ filter (not . null) $ map snd (buildEquivalentWithRegion t)

getActiveFor :: (Rangeable a) => [a] -> a -> [a]
getActiveFor topregionlist targetregion = rv
	where
		disjunct = buildDisjunct topregionlist
		rv = map fst $ filter filt disjunct
		filt (_, r) = Maybe.isJust $ Range.intersection r targetregion


intersectionOfList :: Rangeable a => [a] -> Maybe a
intersectionOfList (r:rs) = foldr folder (Just r) rs
	where
		folder :: Rangeable a => a -> Maybe a -> Maybe a
		folder _ Nothing = Nothing
		folder a (Just mb) = intersection a mb
intersectionOfList [] = Nothing

------------------------------------------------------------------------------

buildDisjunct :: Rangeable a => [a] -> [(a, a)]
buildDisjunct ranges = foldl buildDisjunctFolder [] ranges
	where
		buildDisjunctFolder :: Rangeable a => [(a, a)] -> a -> [(a, a)]
		buildDisjunctFolder curr new = curr ++ (map ( \ n -> (new, n)) (subt [new] (map snd curr)))
		subt :: Rangeable a => [a] -> [a] -> [a]
		subt new curr = foldl diff new curr
		diff :: Rangeable a => [a] -> a -> [a]
		diff rs r = concatMap ( \ n -> difference n r) rs

