
module Map (findExactOrLeft, maybeInsert, findKeyInterval, invert, module Data.Map) where

import qualified Data.Maybe as Maybe
import Data.Map

findExactOrLeft :: (Ord k) => Map k a -> k -> a
findExactOrLeft m k = case exact of
	Nothing -> snd $ findMax left
	Just r -> r
	where
		(left, exact, _) = splitLookup k m

maybeInsert :: Ord k => k -> Maybe a -> Map k a -> Map k a
maybeInsert _ Nothing m = m
maybeInsert k (Just a) m = insert k a m

findKeyInterval :: (Ord k) => k -> k -> Map k a -> Map k a
findKeyInterval k1 k2 m = maybeInsert k2 exact2 left2
	where
		(left1, exact1, _) = splitLookup k1 m
		kinicio = case exact1 of
			Nothing -> fst $ findMax left1
			Just _ -> k1
		(_, exactkinicio, right1) = splitLookup kinicio m
		mapright = insert kinicio (Maybe.fromJust exactkinicio) right1
		(left2, exact2, _) = splitLookup k2 mapright

invert :: (Ord a, Ord k) => Map k a -> Map a k
invert m = fromList listinv
	where
		listorig = toList m
		listinv = Prelude.map ( \ (a, b) -> (b, a) ) listorig

