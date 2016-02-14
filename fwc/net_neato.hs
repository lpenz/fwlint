
module Main where
import Net
import qualified Assertion
import qualified Doc

neato :: [Device] -> String
neato devices = concat [
		"graph {\n",
		"\toverlap=false\n",
		--"\tedge [ len=2 ]\n",
		concatMap dotDevices devices,
		concatMap dotNetworks networks,
		concatMap dotConnections devices,
		concat . fst $ foldr dotInternets ([], 0) devices,
		"}\n"
	]
	where
		interfacesNotInternet device = filter (not . interfaceIsInternet) (getInterfaces device)
		interfaces = (concatMap interfacesNotInternet devices)
		networks = concatMap ( \ i -> getnetwork i) interfaces
			where
				getnetwork :: Interface -> [(String, String, String)]
				getnetwork (Interface _ na@(NetAtom name ip pl)) =
					[(getNetAtomName na,
						if named
							then (concat [ name, "\\n", Doc.showrepr (ip, pl) ])
							else (Doc.showrepr (ip, pl)),
						if named
							then ""
							else "fontsize=\"10\" "
						)]
					where
						named = name /= ""
				getnetwork _ = []
		dotDevices d = concat [ "\t\"", getName d, "\" [ shape=", if isFilter d then "diamond" else "box", " ]\n" ]
		dotNetworks (name, label, fontsize) = concat [ "\t\"", name, "\" [ label=\"", label, "\" ", fontsize, "shape=ellipse ]\n" ]
		dotConnections d = concatMap (dotDeviceInterface d) (interfacesNotInternet d)
		dotDeviceInterface d (Interface _ na)  = concat [ "\t\"", getName d, "\" -- \"", getNetAtomName na,
			"\" \n" ]
		dotInternets :: Device -> ([String], Int) -> ([String], Int)
		dotInternets d (s, n) = (s ++ 
			if deviceHasInternet d then
				[ "\t\"Internet", show n, "\" [ label=\"Internet\" shape=circle ]\n",
				"\t\"", getName d, "\" -- \"Internet", show n, "\"\n" ]
			else [], n + 1)

main :: IO ()
main = do
	interact (neato . Assertion.smartDeviceParser)
	putStrLn ""

