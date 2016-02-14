
module Util where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace
import qualified Doc

invertTuple :: (a, b) -> (b, a)
invertTuple (a, b) = (b, a)

invertMap :: (Ord k, Ord a) => Map.Map k a -> Map.Map a k
invertMap = Map.fromList . (map invertTuple) . Map.toList

(|.) :: (a -> b) -> (b -> c) -> a -> c
(|.) = flip (.)

(|$) :: a -> (a -> b) -> b
(|$) v f = f v

safeHead :: String -> [a] -> a
safeHead _ (a:_) = a
safeHead str [] = error str

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ p ++ [x] ++ s | xs' <- permutations xs, (p, s) <- zip (List.inits xs') (List.tails xs') ]

homogenous :: Eq a => [a] -> Bool
homogenous [] = True
homogenous (_:[]) = True
homogenous (a:as@(_:_)) = and $ map ((==) a) as

trace :: String -> a -> a
trace = Debug.Trace.trace

traceDoc :: Doc.Able a => a -> b -> b
traceDoc a b = Debug.Trace.trace (Doc.showrepr a) b

traceRet :: Doc.Able a => a -> a
traceRet a = Debug.Trace.trace (Doc.showrepr a) a

mapUniq :: (Ord a, Ord b) => (a -> b) -> [a] -> [b]
mapUniq f l0 = Set.toList $ Set.map f $ Set.fromList l0

concatMapUniq :: (Ord a, Ord b) => (a -> [b]) -> [a] -> [b]
concatMapUniq f l0 = Set.toList $ snd rv
	where
		rv = foldr folder (Set.empty, Set.empty) l0
		folder e (se, sr) = if Set.notMember e se then (Set.insert e se, Set.union sr (Set.fromList $ f e)) else (se, sr)

joinTupleList :: (Ord a, Ord b) => [(a, [b])] -> [(a, [b])]
joinTupleList l0 = map ( \ (a, b) -> (a, Set.toList b)) (Map.toList mapa)
	where
		listset = map ( \ (a, b) -> (a, Set.fromList b) ) l0
		mapa = foldr folder Map.empty listset
		folder (a, b) prev = Map.insertWith Set.union a b prev

doubleFilter :: (a -> Bool) -> [a] -> ([a], [a])
doubleFilter f l0 = foldr folder ([], []) l0
	where
		folder e (ltrue, lfalse) = if f e then (ltrue ++ [e], lfalse) else (ltrue, lfalse ++ [e])

const2 :: a -> b -> b
const2 = flip const

fromEither :: Either a b -> b
fromEither (Left _) = error ""
fromEither (Right b) = b

