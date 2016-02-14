
module Main where
import qualified Svg
import qualified Data.List as List
import qualified Map
import qualified Data.Set as Set
import qualified Color
import qualified Rule
import qualified Ip
import qualified Match
import qualified Action
import qualified Assertion
import qualified Net
import qualified Doc
import qualified AnomDistr
import qualified Range

legenda :: [(Color.Color, Color.Color, String)]
legenda = [
	(Color.green, Color.green, show Action.Accept),
	(Color.red, Color.red, show Action.Deny),
	(Color.yellow, Color.yellow, "Partial"),
	(Color.blue, Color.blue, "Disagreement"),
	(Color.white, Color.white, "Not filtered"),
	(Color.gray, Color.gray, "External/internet"),
	(Color.brown, Color.brown, "Absent/undefined")
	]

tuplaFor :: [Net.Device] -> [AnomDistr.AnomDistr] -> AnomDistr.GlobalProfile -> Ip.Addr -> Ip.Addr -> (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String)
tuplaFor devs anoms globalprofile src dst =
	let
		keys = filter ( \ (s, d) -> Net.ipaddrInNetAtom devs src s && Net.ipaddrInNetAtom devs dst d) (Map.keys globalprofile)
		key = head keys
		rules0 = map fst $ snd $ globalprofile Map.! key
		rules = Range.getActiveFor rules0 (Rule.Region (Match.maIps src dst))
		greens = filter ( \ r -> Rule.getAction r == Action.Accept) rules
		alldisamatchs = concatMap getdisamatches anoms
			where
				getdisamatches (AnomDistr.AnomDistrDisagreement _ _ matchs) = matchs
				getdisamatches _ = []
		disagreement = not $ null (filter indisa alldisamatchs)
		indisa match = Match.maMatches match (src, dst)
		corlegend = [
			(null keys, Just Color.brown),
			(Net.ipsOnSameNetworkDevs devs src dst, Nothing),
			(disagreement, Just Color.blue),
			(length greens == 0, Just Color.red),
			(length greens == length rules, Just Color.green),
			(True, Just Color.yellow)
			]
		cor = snd $ head $ filter fst corlegend
		-- cor = Color.red
		-- cor1 = Color.grayer cor
		cor11 = cor
		-- cor11 = if cor /= Color.white && Net.ipaddrInInternet devs src then Color.grayer cor else cor
		cor12 = if cor /= (Just Color.white) && (Net.ipaddrInInternet devs dst || Net.ipaddrInInternet devs src) then (maybe cor (Just . Color.grayer) cor) else cor11
		cor1 = cor12
		cor2 = cor11
		title = if (not $ null keys) then (Just $ Doc.showname $ fst $ globalprofile Map.! key) else Nothing
		descr = Nothing
	in
		(cor1, cor2, title, descr)

showSvgForProfile::[Net.Device] -> (AnomDistr.GlobalProfile, [AnomDistr.AnomDistr], Set.Set (Net.Device, Rule.Rule)) -> String
showSvgForProfile devs (globalprofile, anoms, _) =
	let
		srcs = List.sort $ List.nub $ Net.getImportantIps devs
		dsts = List.sort $ List.nub $ Net.getImportantIps devs
		mapa = (Map.fromList [ ((src, dst), tuplaFor devs anoms globalprofile src dst) | src <- srcs, dst <- dsts ])
		-- (xs, ys) = (srcs, dsts)
		(xs, ys) = Svg.matrixCompress mapa srcs dsts
		(wm, hm) = Svg.matrixSize "Source" xs "Destination" ys
		(wl, hl) = Svg.legendaSize legenda
		width = wm + wl + 10
		height = maximum [hm, hl]
	in
		Svg.header width height ++
		Svg.matrix mapa "Source" xs "Destination" ys ++
		Svg.transform
			((+) 10 $ fst $ Svg.matrixSize "Source" xs "Destination" ys)
			((snd $ Svg.matrixStart "Source" xs "Destination" ys)) 0
			(Svg.legenda legenda)
			++
		Svg.footer

main :: IO ()
main = do
	devstr <- getContents
	let devs = Assertion.smartDeviceParser devstr
	putStrLn $ ((showSvgForProfile devs) . buildGlobalProfileAnomDistr_from_devs) devs
	putStrLn ""
	where
		buildGlobalProfileAnomDistr_from_devs devs = (AnomDistr.buildGlobalProfileAnomDistr devs) $ Net.allPaths devs

