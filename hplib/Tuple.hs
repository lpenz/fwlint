
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Tuple where

e1of3 :: (a, b, c) -> a
e1of3 (a, _, _) = a
e2of3 :: (a, b, c) -> b
e2of3 (_, b, _) = b
e3of3 :: (a, b, c) -> c
e3of3 (_, _, c) = c

class TupleList a b where
	toList :: a -> [b]
	fromList :: [b] -> a
	elem :: Int -> a -> b
	elem i a = (toList a) !! i

instance TupleList (a, a) a where
	toList (a, b) = [a, b]
	fromList (a:b:[]) = (a, b)
	fromList _ = error "Invalid dimension"

instance TupleList (a, a, a) a where
	toList (a, b, c) = [a, b, c]
	fromList (a:b:c:[]) = (a, b, c)
	fromList _ = error "Invalid dimension"


