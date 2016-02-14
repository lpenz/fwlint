
module Svg where

import Data.List
import qualified Data.Map as Map
import qualified Color as Color
import qualified Doc
import qualified Tuple

cellsize :: Int
cellsize = 20

charwidth :: Int
charwidth = 12

charheight::Int
charheight = cellsize

strokeHalfWidth :: Int
strokeHalfWidth = 2

svgLabels::Doc.Able a => [a] -> String
svgLabels l =
	let
		svgLabel:: Doc.Able a => Int -> a -> String
		svgLabel i a = text 0 (i * charheight) (Doc.showrepr a)
	in
		foldl (++) "" (zipWith svgLabel [1..] l)

paint :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	Map.Map (k1, k2) (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String) ->
	((Int, Int), (k1, k2)) -> String
paint mapa ((x, y), k) =
	let
		(cor, corcentro, title, descr) = if Map.notMember k mapa then (Nothing, Nothing, Nothing, Nothing) else mapa Map.! k
		xc = show $ x * cellsize + strokeHalfWidth
		yc = show $ y * cellsize + strokeHalfWidth
	in concat [
		"<rect x=\"", xc, "\" y=\"", yc, "\"",
		" width=\"" , show $ cellsize - 2*strokeHalfWidth, "\"",
		" height=\"", show $ cellsize - 2*strokeHalfWidth, "\"",
		" fill=\""  , maybe "none" ( \ c -> Color.toXML c) corcentro, "\"",
		" stroke=\"", maybe "none" ( \ c -> Color.toXML c) cor, "\"",
		" stroke-width=\"", show $ 2*strokeHalfWidth, "\"",
		">",
		(case title of
			Nothing -> ""
			Just t -> "<title>" ++ t ++ "</title>"),
		(case descr of
			Nothing -> ""
			Just d -> "<desc>" ++ d ++ "</desc>"),
		"</rect>\n",
		"<rect x=\"", (show $ x * cellsize), "\" y=\"", (show $ y * cellsize), "\"",
		" width=\"" , show cellsize, "\"",
		" height=\"", show cellsize, "\"",
		" fill=\"none\" stroke=\"#000000\"",
		"/>\n"
		]

header :: Int -> Int -> String
header width height = concat [
	"<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n",
	"<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\"\n",
	"    \"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">\n",
	"    <svg xmlns=\"http://www.w3.org/2000/svg\"\n",
	"    xmlns:xlink=\"http://www.w3.org/1999/xlink\" onload=\"Init(evt)\" xml:space=\"preserve\"\n",
	"    zoomAndPan=\"disable\"\n",
	"    width=\"", show width, "\" height=\"", show height, "\" viewBox=\"0 0 ", show width, " ", show height, "\"",
	">\n",
	--"<script type=\"text/ecmascript\"><![CDATA[\n",
	--"    var SVGDocument = null;\n",
	--"    var SVGRoot = null;\n",
	--"    var SVGViewBox = null;\n",
	--"    var svgns = 'http://www.w3.org/2000/svg';\n",
	--"    var xlinkns = 'http://www.w3.org/1999/xlink';\n",
	--"    var toolTip = null;\n",
	--"    var TrueCoords = null;\n",
	--"    var tipBox = null;\n",
	--"    var tipText = null;\n",
	--"    var tipTitle = null;\n",
	--"    var tipDesc = null;\n",
	--"\n",
	--"    var lastElement = null;\n",
	--"    var titleText = '';\n",
	--"    var titleDesc = '';\n",
	--"\n",
	--"\n",
	--"    function Init(evt)\n",
	--"    {\n",
	--"       SVGDocument = evt.target.ownerDocument;\n",
	--"       SVGRoot = SVGDocument.documentElement;\n",
	--"       TrueCoords = SVGRoot.createSVGPoint();\n",
	--"\n",
	--"       toolTip = SVGDocument.getElementById('ToolTip');\n",
	--"       tipBox = SVGDocument.getElementById('tipbox');\n",
	--"       tipText = SVGDocument.getElementById('tipText');\n",
	--"       tipTitle = SVGDocument.getElementById('tipTitle');\n",
	--"       tipDesc = SVGDocument.getElementById('tipDesc');\n",
	--"       //window.status = (TrueCoords);\n",
	--"\n",
	--"       //create event for object\n",
	--"       SVGRoot.addEventListener('mousemove', ShowTooltip, false);\n",
	--"       SVGRoot.addEventListener('mouseout', HideTooltip, false);\n",
	--"    };\n",
	--"\n",
	--"\n",
	--"    function GetTrueCoords(evt)\n",
	--"    {\n",
	--"       // find the current zoom level and pan setting, and adjust the reported\n",
	--"       //    mouse position accordingly\n",
	--"       var newScale = SVGRoot.currentScale;\n",
	--"       var translation = SVGRoot.currentTranslate;\n",
	--"       TrueCoords.x = (evt.clientX - translation.x)/newScale;\n",
	--"       TrueCoords.y = (evt.clientY - translation.y)/newScale;\n",
	--"    };\n",
	--"\n",
	--"\n",
	--"    function HideTooltip( evt )\n",
	--"    {\n",
	--"       toolTip.setAttributeNS(null, 'visibility', 'hidden');\n",
	--"    };\n",
	--"\n",
	--"\n",
	--"    function ShowTooltip( evt )\n",
	--"    {\n",
	--"       GetTrueCoords( evt );\n",
	--"\n",
	--"       var tipScale = 1/SVGRoot.currentScale;\n",
	--"       var textWidth = 0;\n",
	--"       var tspanWidth = 0;\n",
	--"       var boxHeight = 20;\n",
	--"\n",
	--"       tipBox.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' );\n",
	--"       tipText.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' );\n",
	--"\n",
	--"       var titleValue = '';\n",
	--"       var descValue = '';\n",
	--"       var targetElement = evt.target;\n",
	--"       if ( lastElement != targetElement )\n",
	--"       {\n",
	--"          var targetTitle = targetElement.getElementsByTagName('title').item(0);\n",
	--"          if ( targetTitle && targetTitle.firstChild )\n",
	--"          {\n",
	--"             // if there is a 'title' element, use its contents for the tooltip title\n",
	--"             titleValue = targetTitle.firstChild.nodeValue;\n",
	--"          }\n",
	--"\n",
	--"          var targetDesc = targetElement.getElementsByTagName('desc').item(0);\n",
	--"          if ( targetDesc )\n",
	--"          {\n",
	--"             // if there is a 'desc' element, use its contents for the tooltip desc\n",
	--"             descValue = targetDesc.firstChild.nodeValue;\n",
	--"\n",
	--"             if ( '' == titleValue )\n",
	--"             {\n",
	--"                // if there is no 'title' element, use the contents of the 'desc' element for the tooltip title instead\n",
	--"                titleValue = descValue;\n",
	--"                descValue = '';\n",
	--"             }\n",
	--"          }\n",
	--"\n",
	--"          // if there is still no 'title' element, use the contents of the 'id' attribute for the tooltip title\n",
	--"          if ( '' == titleValue )\n",
	--"          {\n",
	--"             titleValue = targetElement.getAttributeNS(null, 'id');\n",
	--"          }\n",
	--"\n",
	--"          // selectively assign the tooltip title and desc the proper values,\n",
	--"          //   and hide those which don't have text values\n",
	--"          //\n",
	--"          var titleDisplay = 'none';\n",
	--"          if ( '' != titleValue )\n",
	--"          {\n",
	--"             tipTitle.firstChild.nodeValue = titleValue;\n",
	--"             titleDisplay = 'inline';\n",
	--"          }\n",
	--"          tipTitle.setAttributeNS(null, 'display', titleDisplay );\n",
	--"\n",
	--"\n",
	--"          var descDisplay = 'none';\n",
	--"          if ( '' != descValue )\n",
	--"          {\n",
	--"             tipDesc.firstChild.nodeValue = descValue;\n",
	--"             descDisplay = 'inline';\n",
	--"          }\n",
	--"          tipDesc.setAttributeNS(null, 'display', descDisplay );\n",
	--"       }\n",
	--"\n",
	--"       // if there are tooltip contents to be displayed, adjust the size and position of the box\n",
	--"       if ( '' != titleValue )\n",
	--"       {\n",
	--"          var xPos = TrueCoords.x + (10 * tipScale);\n",
	--"          var yPos = TrueCoords.y + (10 * tipScale);\n",
	--"\n",
	--"          //return rectangle around text as SVGRect object\n",
	--"          var outline = tipText.getBBox();\n",
	--"          tipBox.setAttributeNS(null, 'width', Number(outline.width) + 10);\n",
	--"          tipBox.setAttributeNS(null, 'height', Number(outline.height) + 10);\n",
	--"\n",
	--"          // update position\n",
	--"          toolTip.setAttributeNS(null, 'transform', 'translate(' + xPos + ',' + yPos + ')');\n",
	--"          toolTip.setAttributeNS(null, 'visibility', 'visible');\n",
	--"       }\n",
	--"    };\n",
	--"\n",
	--" ]]></script>\n",
	"<rect x=\"0\" y=\"0\"",
	" width=\"" , show width, "\"",
	" height=\"", show height, "\"",
	" fill=\"white\"", " stroke=\"white\"",
	"/>" ]

footer :: String
footer = concat [
	--"<g id='ToolTip' opacity='0.8' visibility='hidden' pointer-events='none'>\n",
	--"\t<rect id='tipbox' x='2' y='5' width='88' height='20' rx='2' ry='2' fill='white'/>\n",
	--"\t<text id='tipText' x='5' y='20' font-size='12'>\n",
	--"\t\t<tspan id='tipTitle' x='5' font-weight='bold'><![CDATA[]]></tspan>\n",
	--"\t\t<tspan id='tipDesc' x='5' dy='15' fill='blue'><![CDATA[]]></tspan>\n",
	--"\t</text>\n",
	--"</g>\n",
	"</svg>\n" ]

transform :: Int -> Int -> Int -> String -> String
transform tx ty r inside =
	concat [ "<g transform=\" translate(", show tx, ", ", show ty, ") rotate(", show r, ")\">\n", inside, "</g>\n" ]

text :: Int -> Int -> String -> String
text x y str = concat [ "<text font-size=\"20\" x=\"", show x, "\" y=\"", show y, "\">", str, "</text>\n" ]

matrixCompress :: (Doc.Able kx, Eq kx, Ord kx, Doc.Able ky, Eq ky, Ord ky) =>
	Map.Map (kx, ky) (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String) ->
	[kx] ->
	[ky] ->
	([kx], [ky])
matrixCompress mapa xs0 ys0 = (xsf, ysf)
	where
		gettrio l = zip3 l (tail l) (tail $ tail l)
		doit l f = (head l):(map Tuple.e2of3 (filter f (gettrio l))) ++ [last l]
		fx (x1, x2, x3) = rv
			where
				eqs y = mapa Map.! (x1, y) == mapa Map.! (x2, y) && mapa Map.! (x3, y) == mapa Map.! (x2, y)
				rv = not $ and $ map eqs ys0
		fy (y1, y2, y3) = rv
			where
				eqs x = mapa Map.! (x, y1) == mapa Map.! (x, y2) && mapa Map.! (x, y3) == mapa Map.! (x, y2)
				rv = not $ and $ map eqs xs0
		xsf = if length xs0 > 3 then doit xs0 fx else xs0
		ysf = if length ys0 > 3 then doit ys0 fy else ys0
		

matrixOnly :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	Map.Map (k1, k2) (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String) ->
	[k1] ->
	[k2] ->
	String
matrixOnly mapa xs ys =
	let
		matrixwidth  = length xs * cellsize
		matrixheight = length ys * cellsize
		m = "<rect x=\"0\" y=\"0\" fill=\"none\" stroke=\"black\" width=\"" ++ show matrixwidth ++ "\" height=\"" ++ show matrixheight ++ "\"/>\n" ++
			concatMap (paint mapa) [ ((x, y), (xs !! x, ys !! y)) | x <- [0 .. length xs - 1], y <- [0 .. length ys - 1]]
	in
		m

matrix :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	Map.Map (k1, k2) (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String) ->
	String -> 
	[k1] ->
	String ->
	[k2] ->
	String
matrix mapa xlabel xs ylabel ys =
	let
		xstartgraph = (maximum (length ylabel:(map (length.Doc.showrepr) (ys))) + 3)* charwidth
		ystartgraph = (maximum (length xlabel:(map (length.Doc.showrepr) (xs))))* charwidth
		matrixwidth  = length xs * cellsize
		matrixheight = length ys * cellsize
		widthtotal  = show (xstartgraph + matrixwidth + 1)
		heighttotal = show (ystartgraph + matrixheight + 1)
		ylabels = transform 0 (ystartgraph - 2) 0 (
			text 0 0 ylabel ++
			svgLabels ys)
		xlabels = transform (xstartgraph -2) (ystartgraph - 2) (-90) (
			text 0 0 xlabel ++
			svgLabels xs)
		m = transform xstartgraph ystartgraph 0 (matrixOnly mapa xs ys)
	in
		"<rect x=\"0\" y=\"0\" fill=\"none\" width=\"" ++ widthtotal ++ "\" height=\"" ++ heighttotal ++ "\"><title></title></rect>\n" ++
		ylabels ++
		xlabels ++
		m

matrixSizeNoLabel :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	[k1] ->
	[k2] ->
	(Int, Int)
matrixSizeNoLabel xs ys =
	let
		matrixwidth  = length xs * cellsize
		matrixheight = length ys * cellsize
	in
		(matrixwidth, matrixheight)


matrixSize :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	String -> 
	[k1] ->
	String ->
	[k2] ->
	(Int, Int)
matrixSize xlabel xs ylabel ys =
	let
		xstartgraph = (maximum (length ylabel:(map (length.Doc.showrepr) (ys))) + 3) * charwidth
		ystartgraph = (maximum (length xlabel:(map (length.Doc.showrepr) (xs)))) * charwidth
		(matrixwidth, matrixheight) = matrixSizeNoLabel xs ys
		widthtotal  = xstartgraph + matrixwidth + 1
		heighttotal = ystartgraph + matrixheight + 1
	in
		(widthtotal, heighttotal)

matrixStart :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	String -> 
	[k1] ->
	String ->
	[k2] ->
	(Int, Int)
matrixStart xlabel xs ylabel ys =
	let
		xstartgraph = (maximum (length ylabel:(map (length.Doc.showrepr) (ys)))) * charwidth
		ystartgraph = (maximum (length xlabel:(map (length.Doc.showrepr) (xs)))) * charwidth
	in
		(xstartgraph, ystartgraph)

legenda :: [(Color.Color, Color.Color, String)] -> String
legenda leg =
	let
		margin = 5
		rect :: Int -> Color.Color -> Color.Color -> String
		rect y c cc = concat [
			"<rect x=\"", show $ margin + strokeHalfWidth, "\" y=\"", show $ y + strokeHalfWidth, "\"",
			" width=\"" , show $ Svg.cellsize - 2*strokeHalfWidth, "\"",
			" height=\"", show $ Svg.cellsize - 2*strokeHalfWidth, "\"",
			" fill=\"", Color.toXML cc , "\"", " stroke=\"", Color.toXML c, "\" stroke-width=\"", show $ 2*strokeHalfWidth, "\"",
			"/>" ]
		entrada :: (Int, (Color.Color, Color.Color, String)) -> String
		entrada (y, (c, cc, str)) =
			(rect (y + margin) c cc) ++ text (Svg.cellsize + margin * 2) (y + Svg.charheight + 2) str
		width = Svg.cellsize + 2*margin + Svg.charwidth * (maximum $ map (length.( \ (_, _, str) -> str)) leg)
		height = (length leg) * (Svg.cellsize + margin) + margin
		frame = concat [
			"<rect x=\"0\" y=\"0\"",
			" width=\"" , show width, "\"",
			" height=\"", show height, "\"",
			" fill=\"white\"",
			" stroke=\"black\"",
			"/>" ]
		legpos = zip [0,(Svg.cellsize + margin)..] leg
	in
		frame ++ concatMap entrada legpos

legendaSize :: [(Color.Color, Color.Color, String)] -> (Int, Int)
legendaSize leg =
	let
		margin = 5
		width = Svg.cellsize + 2*margin + Svg.charwidth * (maximum $ map (length.( \ (_, _, str) -> str)) leg)
		height = (length leg) * (Svg.cellsize + margin) + margin
	in
		(width, height)

build :: (Doc.Able k1, Eq k1, Ord k1, Doc.Able k2, Eq k2, Ord k2) =>
	Map.Map (k1, k2) (Maybe Color.Color, Maybe Color.Color, Maybe String, Maybe String) ->
	String -> 
	[k1] ->
	String ->
	[k2] ->
	String
build mapa xlabel xs ylabel ys =
	header width height ++
	matrix mapa xlabel xs ylabel ys ++
	footer
	where 
	(width, height) = matrixSize xlabel xs ylabel ys

