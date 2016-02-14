
module Main where
import qualified Data.Map as Map
import qualified Data.List as List
import Color as Color
import Svg
import qualified Doc

newtype MyInt = MyInt Int deriving (Eq, Ord, Enum)
instance Show MyInt where
	show _ = ""
instance Read MyInt where
    readsPrec p1 input = map ( \ (i, str) -> (MyInt i, str) ) $ readsPrec p1 input
instance Doc.Able MyInt where
	repr _ = Doc.empty

newtype Rule = Rule (Int, MyInt, MyInt, Cor) deriving (Eq, Ord, Show, Read)
instance Doc.Able Rule where
	repr _ = Doc.empty
	--repr (Rule (i, _, _, _)) = Doc.text $ "R" ++ show i

data Cor = Blue | Orange | Red | Green deriving (Eq, Ord, Show, Read)

cor2color :: Cor -> Color
cor2color Blue = blue
cor2color Orange = orange
cor2color Red = red
cor2color Green = green

ruleCell :: Rule -> (Maybe Color, Maybe Color, Maybe String, Maybe String)
ruleCell (Rule (_, _, _, cor)) = (Just color, Just color, Nothing, Nothing)
	where color = cor2color cor

rulesSvg :: [Rule] -> String
rulesSvg rules =
	let
		xslist = concatMap ( \ (Rule (_, ini, fim, _)) -> [ini, fim]) rules
		xs = [minimum xslist .. maximum xslist]
		ys = rules
		cm = concatMap ( \ r@(Rule (_, ini, fim, _)) -> [((x, r), ruleCell r) | x <- [ini .. fim]]) rules
		mapa = Map.fromList cm
		(wm, hm) = Svg.matrixSize "" xs "" ys
	in
		concat [
			Svg.header wm hm,
			Svg.matrix mapa "" xs "" ys,
			Svg.footer
			]

main :: IO ()
main = do
	interact (rulesSvg . read)

