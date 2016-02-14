
module IntegerIsomorph where

class Ord t => IntegerIsomorph t where
	toIntegerSpace :: t -> Integer
	fromIntegerSpace :: Integer -> t

instance IntegerIsomorph Integer where
	toIntegerSpace = id
	fromIntegerSpace = id

