
module Main where
import qualified Ip
import qualified Svg
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Color as Color
import Rule
import qualified MatchAction
import qualified Parse
import qualified Range
import qualified Doc

colorOfRule :: [Rule] -> Rule -> (Maybe Color, Maybe Color)
colorOfRule rs r@(Rule rname _ ract) = if r == (last rs) then (Just Color.red, Just Color.red) else (tcolor, color)
	where
		rulesindex = zip rs $ List.filter notspecial Color.gimmeList
		notspecial c = c /= Color.red && c /= Color.green
		color = Just $ snd $ head $ List.filter (\ (ri@(Rule riname _ _), _) -> ri == r && riname == rname) rulesindex
		tcolor = Just $ if ract == MatchAction.Accept then Color.green else Color.red
colorOfRule _ _ = error "no color for region"

tuplaFor :: [Rule] -> (Rule -> (Maybe Color, Maybe Color)) -> Ip.Addr -> Ip.Addr -> (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String)
tuplaFor rules colorizer src dst = 
	let
		rs = List.filter ( \ r -> Range.match r [0, src, 0, dst, 0] ) rules
		top = head rs
		(cor, ccor) = colorizer top
		title = Just $ Doc.showrepr top
		descr = Just $ concatMap (\ r -> Doc.showrepr r ++ ", ") rs
	in
		(cor, ccor, title, descr)

showSvgForFilter::[Rule] -> String
showSvgForFilter rules =
	let
		srcs = List.concatMap Range.importantElements (List.map ruleGetSrc rules)
		dsts = List.concatMap Range.importantElements (List.map ruleGetDst rules)
		mapa = (Map.fromList [ ((src, dst), tuplaFor rules (colorOfRule rules) src dst) | src <- srcs, dst <- dsts ])
		xs = (sort $ nub $ srcs)
		ys = (sort $ nub $ dsts)
		(wm, hm) = Svg.matrixSize "Source" xs "Destination" ys
		listmap = ( \ r -> (Maybe.fromJust $ fst $ colorOfRule rules r, Maybe.fromJust $ snd $ colorOfRule rules r, Doc.showrepr r))
		(wl, hl) = Svg.legendaSize (List.map listmap (rules))
		width = wm + wl + 10
		height = maximum [hm, hl]
	in
		Svg.header width height ++
		Svg.matrix mapa "Source" xs "Destination" ys ++
		Svg.transform
			((+) 10 $ fst $ Svg.matrixSize "Source" xs "Destination" ys)
			((snd $ Svg.matrixStart "Source" xs "Destination" ys)) 0
			(Svg.legenda (List.map listmap (rules)))
			++
		Svg.footer

main :: IO ()
main = do
	interact (showSvgForFilter . Parse.runParser)

