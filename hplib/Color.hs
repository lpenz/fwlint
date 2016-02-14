
module Color where

import Text.Printf
import qualified Doc

data Color = Color { cred::Int, cgreen::Int, cblue::Int } deriving (Eq, Show, Read, Ord)

instance Doc.Able Color

toTuple :: Color -> (Int, Int, Int)
toTuple (Color r g b) = (r, g, b)

fromTuple :: (Int, Int, Int) -> Color
fromTuple (r, g, b) = Color r g b

darkgrey :: Color
darkgrey = Color 63 63 63

lightgrey :: Color
lightgrey = Color (127+63) (127+63) (127+63)

toXML :: Color -> String
toXML (Color r g b) = printf "#%02x%02x%02x" r g b

gimmeList :: [Color]
gimmeList = cycle [
	Color { cred = 0xFF, cgreen = 0x00, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0xFF, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x00, cblue = 0xFF },
	Color { cred = 0x00, cgreen = 0xFF, cblue = 0xFF },
	Color { cred = 0xFF, cgreen = 0x00, cblue = 0xFF },
	Color { cred = 0xFF, cgreen = 0xFF, cblue = 0x00 },
	Color { cred = 0x7F, cgreen = 0x00, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x7F, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x00, cblue = 0x7F },
	Color { cred = 0x00, cgreen = 0x7F, cblue = 0x7F },
	Color { cred = 0x7F, cgreen = 0x00, cblue = 0x7F },
	Color { cred = 0x7F, cgreen = 0x7F, cblue = 0x00 },
	Color { cred = 0x3F, cgreen = 0x00, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x3F, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x00, cblue = 0x3F },
	Color { cred = 0x00, cgreen = 0x3F, cblue = 0x3F },
	Color { cred = 0x3F, cgreen = 0x00, cblue = 0x3F },
	Color { cred = 0x3F, cgreen = 0x3F, cblue = 0x00 },
	Color { cred = 0xBE, cgreen = 0x00, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0xBE, cblue = 0x00 },
	Color { cred = 0x00, cgreen = 0x00, cblue = 0xBE },
	Color { cred = 0x00, cgreen = 0xBE, cblue = 0xBE },
	Color { cred = 0xBE, cgreen = 0x00, cblue = 0xBE },
	Color { cred = 0xBE, cgreen = 0xBE, cblue = 0x00 }
	]

grayer :: Color -> Color
grayer cor = fromHSV (h, s, v)
       where
               (h, s0, v0) = toHSV cor
               s = s0 * 0.75
               v = v0 * 0.75

toHSV :: Color -> (Rational, Rational, Rational)
toHSV (Color r0 g0 b0) = (h, s, v)
	where
		(r, g, b) = (toRational r0 / 255, toRational g0 / 255, toRational b0 / 255)
		minc = minimum [ r, g, b ]
		maxc = maximum [ r, g, b ]
		v = maxc
		delta = maxc - minc
		s = if maxc == 0 then 0 else (delta / maxc)
		h =
			if maxc == 0
				then -1
				else 60 * (if r == maxc then ((g - b)/delta)
				else if g == maxc then (2 + (b - r) / delta)
				else (4 + (r - g) / delta))

fromHSV :: (Rational, Rational, Rational) -> Color
fromHSV (_, 0, v0) = fromTuple (v, v, v)
	where
		v = round (v0 * 255)
fromHSV (h0, s, v) = fromTuple (round (r * 255), round (g * 255), round (b * 255))
	where
		h = h0 / 60
		i = toRational (floor (fromRational h))
		f = h - i
		p = v * (1 - s)
		q = v * (1 - s * f)
		t = v * (1 - s * (1 - f))
		(r, g, b) = case i of
			0 -> (v, t, p)
			1 -> (q, v, p)
			2 -> (p, v, t)
			3 -> (p, q, v)
			4 -> (t, p, v)
			_ -> (v, p, q)
		
-- Cores do X:

snow :: Color
snow                   = Color 255 250 250 
ghost_white :: Color
ghost_white            = Color 248 248 255 
ghostWhite :: Color
ghostWhite             = Color 248 248 255 
white_smoke :: Color
white_smoke            = Color 245 245 245 
whiteSmoke :: Color
whiteSmoke             = Color 245 245 245 
gainsboro :: Color
gainsboro              = Color 220 220 220 
floral_white :: Color
floral_white           = Color 255 250 240 
floralWhite :: Color
floralWhite            = Color 255 250 240 
old_lace :: Color
old_lace               = Color 253 245 230 
oldLace :: Color
oldLace                = Color 253 245 230 
linen :: Color
linen                  = Color 250 240 230 
antique_white :: Color
antique_white          = Color 250 235 215 
antiqueWhite :: Color
antiqueWhite           = Color 250 235 215 
papaya_whip :: Color
papaya_whip            = Color 255 239 213 
papayaWhip :: Color
papayaWhip             = Color 255 239 213 
blanched_almond :: Color
blanched_almond        = Color 255 235 205 
blanchedAlmond :: Color
blanchedAlmond         = Color 255 235 205 
bisque :: Color
bisque                 = Color 255 228 196 
peach_puff :: Color
peach_puff             = Color 255 218 185 
peachPuff :: Color
peachPuff              = Color 255 218 185 
navajo_white :: Color
navajo_white           = Color 255 222 173 
navajoWhite :: Color
navajoWhite            = Color 255 222 173 
moccasin :: Color
moccasin               = Color 255 228 181 
cornsilk :: Color
cornsilk               = Color 255 248 220 
ivory :: Color
ivory                  = Color 255 255 240 
lemon_chiffon :: Color
lemon_chiffon          = Color 255 250 205 
lemonChiffon :: Color
lemonChiffon           = Color 255 250 205 
seashell :: Color
seashell               = Color 255 245 238 
honeydew :: Color
honeydew               = Color 240 255 240 
mint_cream :: Color
mint_cream             = Color 245 255 250 
mintCream :: Color
mintCream              = Color 245 255 250 
azure :: Color
azure                  = Color 240 255 255 
alice_blue :: Color
alice_blue             = Color 240 248 255 
aliceBlue :: Color
aliceBlue              = Color 240 248 255 
lavender :: Color
lavender               = Color 230 230 250 
lavender_blush :: Color
lavender_blush         = Color 255 240 245 
lavenderBlush :: Color
lavenderBlush          = Color 255 240 245 
misty_rose :: Color
misty_rose             = Color 255 228 225 
mistyRose :: Color
mistyRose              = Color 255 228 225 
white :: Color
white                  = Color 255 255 255 
black :: Color
black                  = Color   0   0   0 
dark_slate_gray :: Color
dark_slate_gray        = Color  47  79  79 
darkSlateGray :: Color
darkSlateGray          = Color  47  79  79 
dark_slate_grey :: Color
dark_slate_grey        = Color  47  79  79 
darkSlateGrey :: Color
darkSlateGrey          = Color  47  79  79 
dim_gray :: Color
dim_gray               = Color 105 105 105 
dimGray :: Color
dimGray                = Color 105 105 105 
dim_grey :: Color
dim_grey               = Color 105 105 105 
dimGrey :: Color
dimGrey                = Color 105 105 105 
slate_gray :: Color
slate_gray             = Color 112 128 144 
slateGray :: Color
slateGray              = Color 112 128 144 
slate_grey :: Color
slate_grey             = Color 112 128 144 
slateGrey :: Color
slateGrey              = Color 112 128 144 
light_slate_gray :: Color
light_slate_gray       = Color 119 136 153 
lightSlateGray :: Color
lightSlateGray         = Color 119 136 153 
light_slate_grey :: Color
light_slate_grey       = Color 119 136 153 
lightSlateGrey :: Color
lightSlateGrey         = Color 119 136 153 
gray :: Color
gray                   = Color 190 190 190 
grey :: Color
grey                   = Color 190 190 190 
light_grey :: Color
light_grey             = Color 211 211 211 
lightGrey :: Color
lightGrey              = Color 211 211 211 
light_gray :: Color
light_gray             = Color 211 211 211 
lightGray :: Color
lightGray              = Color 211 211 211 
midnight_blue :: Color
midnight_blue          = Color  25  25 112 
midnightBlue :: Color
midnightBlue           = Color  25  25 112 
navy :: Color
navy                   = Color   0   0 128 
navy_blue :: Color
navy_blue              = Color   0   0 128 
navyBlue :: Color
navyBlue               = Color   0   0 128 
cornflower_blue :: Color
cornflower_blue        = Color 100 149 237 
cornflowerBlue :: Color
cornflowerBlue         = Color 100 149 237 
dark_slate_blue :: Color
dark_slate_blue        = Color  72  61 139 
darkSlateBlue :: Color
darkSlateBlue          = Color  72  61 139 
slate_blue :: Color
slate_blue             = Color 106  90 205 
slateBlue :: Color
slateBlue              = Color 106  90 205 
medium_slate_blue :: Color
medium_slate_blue      = Color 123 104 238 
mediumSlateBlue :: Color
mediumSlateBlue        = Color 123 104 238 
light_slate_blue :: Color
light_slate_blue       = Color 132 112 255 
lightSlateBlue :: Color
lightSlateBlue         = Color 132 112 255 
medium_blue :: Color
medium_blue            = Color   0   0 205 
mediumBlue :: Color
mediumBlue             = Color   0   0 205 
royal_blue :: Color
royal_blue             = Color  65 105 225 
royalBlue :: Color
royalBlue              = Color  65 105 225 
blue :: Color
blue                   = Color   0   0 255 
dodger_blue :: Color
dodger_blue            = Color  30 144 255 
dodgerBlue :: Color
dodgerBlue             = Color  30 144 255 
deep_sky_blue :: Color
deep_sky_blue          = Color   0 191 255 
deepSkyBlue :: Color
deepSkyBlue            = Color   0 191 255 
sky_blue :: Color
sky_blue               = Color 135 206 235 
skyBlue :: Color
skyBlue                = Color 135 206 235 
light_sky_blue :: Color
light_sky_blue         = Color 135 206 250 
lightSkyBlue :: Color
lightSkyBlue           = Color 135 206 250 
steel_blue :: Color
steel_blue             = Color  70 130 180 
steelBlue :: Color
steelBlue              = Color  70 130 180 
light_steel_blue :: Color
light_steel_blue       = Color 176 196 222 
lightSteelBlue :: Color
lightSteelBlue         = Color 176 196 222 
light_blue :: Color
light_blue             = Color 173 216 230 
lightBlue :: Color
lightBlue              = Color 173 216 230 
powder_blue :: Color
powder_blue            = Color 176 224 230 
powderBlue :: Color
powderBlue             = Color 176 224 230 
pale_turquoise :: Color
pale_turquoise         = Color 175 238 238 
paleTurquoise :: Color
paleTurquoise          = Color 175 238 238 
dark_turquoise :: Color
dark_turquoise         = Color   0 206 209 
darkTurquoise :: Color
darkTurquoise          = Color   0 206 209 
medium_turquoise :: Color
medium_turquoise       = Color  72 209 204 
mediumTurquoise :: Color
mediumTurquoise        = Color  72 209 204 
turquoise :: Color
turquoise              = Color  64 224 208 
cyan :: Color
cyan                   = Color   0 255 255 
light_cyan :: Color
light_cyan             = Color 224 255 255 
lightCyan :: Color
lightCyan              = Color 224 255 255 
cadet_blue :: Color
cadet_blue             = Color  95 158 160 
cadetBlue :: Color
cadetBlue              = Color  95 158 160 
medium_aquamarine :: Color
medium_aquamarine      = Color 102 205 170 
mediumAquamarine :: Color
mediumAquamarine       = Color 102 205 170 
aquamarine :: Color
aquamarine             = Color 127 255 212 
dark_green :: Color
dark_green             = Color   0 100   0 
darkGreen :: Color
darkGreen              = Color   0 100   0 
dark_olive_green :: Color
dark_olive_green       = Color  85 107  47 
darkOliveGreen :: Color
darkOliveGreen         = Color  85 107  47 
dark_sea_green :: Color
dark_sea_green         = Color 143 188 143 
darkSeaGreen :: Color
darkSeaGreen           = Color 143 188 143 
sea_green :: Color
sea_green              = Color  46 139  87 
seaGreen :: Color
seaGreen               = Color  46 139  87 
medium_sea_green :: Color
medium_sea_green       = Color  60 179 113 
mediumSeaGreen :: Color
mediumSeaGreen         = Color  60 179 113 
light_sea_green :: Color
light_sea_green        = Color  32 178 170 
lightSeaGreen :: Color
lightSeaGreen          = Color  32 178 170 
pale_green :: Color
pale_green             = Color 152 251 152 
paleGreen :: Color
paleGreen              = Color 152 251 152 
spring_green :: Color
spring_green           = Color   0 255 127 
springGreen :: Color
springGreen            = Color   0 255 127 
lawn_green :: Color
lawn_green             = Color 124 252   0 
lawnGreen :: Color
lawnGreen              = Color 124 252   0 
green :: Color
green                  = Color   0 255   0 
chartreuse :: Color
chartreuse             = Color 127 255   0 
medium_spring_green :: Color
medium_spring_green    = Color   0 250 154 
mediumSpringGreen :: Color
mediumSpringGreen      = Color   0 250 154 
green_yellow :: Color
green_yellow           = Color 173 255  47 
greenYellow :: Color
greenYellow            = Color 173 255  47 
lime_green :: Color
lime_green             = Color  50 205  50 
limeGreen :: Color
limeGreen              = Color  50 205  50 
yellow_green :: Color
yellow_green           = Color 154 205  50 
yellowGreen :: Color
yellowGreen            = Color 154 205  50 
forest_green :: Color
forest_green           = Color  34 139  34 
forestGreen :: Color
forestGreen            = Color  34 139  34 
olive_drab :: Color
olive_drab             = Color 107 142  35 
oliveDrab :: Color
oliveDrab              = Color 107 142  35 
dark_khaki :: Color
dark_khaki             = Color 189 183 107 
darkKhaki :: Color
darkKhaki              = Color 189 183 107 
khaki :: Color
khaki                  = Color 240 230 140 
pale_goldenrod :: Color
pale_goldenrod         = Color 238 232 170 
paleGoldenrod :: Color
paleGoldenrod          = Color 238 232 170 
light_goldenrod_yellow :: Color
light_goldenrod_yellow = Color 250 250 210 
lightGoldenrodYellow :: Color
lightGoldenrodYellow   = Color 250 250 210 
light_yellow :: Color
light_yellow           = Color 255 255 224 
lightYellow :: Color
lightYellow            = Color 255 255 224 
yellow :: Color
yellow                 = Color 255 255   0 
gold :: Color
gold                   = Color 255 215   0 
light_goldenrod :: Color
light_goldenrod        = Color 238 221 130 
lightGoldenrod :: Color
lightGoldenrod         = Color 238 221 130 
goldenrod :: Color
goldenrod              = Color 218 165  32 
dark_goldenrod :: Color
dark_goldenrod         = Color 184 134  11 
darkGoldenrod :: Color
darkGoldenrod          = Color 184 134  11 
rosy_brown :: Color
rosy_brown             = Color 188 143 143 
rosyBrown :: Color
rosyBrown              = Color 188 143 143 
indian_red :: Color
indian_red             = Color 205  92  92 
indianRed :: Color
indianRed              = Color 205  92  92 
saddle_brown :: Color
saddle_brown           = Color 139  69  19 
saddleBrown :: Color
saddleBrown            = Color 139  69  19 
sienna :: Color
sienna                 = Color 160  82  45 
peru :: Color
peru                   = Color 205 133  63 
burlywood :: Color
burlywood              = Color 222 184 135 
beige :: Color
beige                  = Color 245 245 220 
wheat :: Color
wheat                  = Color 245 222 179 
sandy_brown :: Color
sandy_brown            = Color 244 164  96 
sandyBrown :: Color
sandyBrown             = Color 244 164  96 
tan :: Color
tan                    = Color 210 180 140 
chocolate :: Color
chocolate              = Color 210 105  30 
firebrick :: Color
firebrick              = Color 178  34  34 
brown :: Color
brown                  = Color 165  42  42 
dark_salmon :: Color
dark_salmon            = Color 233 150 122 
darkSalmon :: Color
darkSalmon             = Color 233 150 122 
salmon :: Color
salmon                 = Color 250 128 114 
light_salmon :: Color
light_salmon           = Color 255 160 122 
lightSalmon :: Color
lightSalmon            = Color 255 160 122 
orange :: Color
orange                 = Color 255 165   0 
dark_orange :: Color
dark_orange            = Color 255 140   0 
darkOrange :: Color
darkOrange             = Color 255 140   0 
coral :: Color
coral                  = Color 255 127  80 
light_coral :: Color
light_coral            = Color 240 128 128 
lightCoral :: Color
lightCoral             = Color 240 128 128 
tomato :: Color
tomato                 = Color 255  99  71 
orange_red :: Color
orange_red             = Color 255  69   0 
orangeRed :: Color
orangeRed              = Color 255  69   0 
red :: Color
red                    = Color 255   0   0 
hot_pink :: Color
hot_pink               = Color 255 105 180 
hotPink :: Color
hotPink                = Color 255 105 180 
deep_pink :: Color
deep_pink              = Color 255  20 147 
deepPink :: Color
deepPink               = Color 255  20 147 
pink :: Color
pink                   = Color 255 192 203 
light_pink :: Color
light_pink             = Color 255 182 193 
lightPink :: Color
lightPink              = Color 255 182 193 
pale_violet_red :: Color
pale_violet_red        = Color 219 112 147 
paleVioletRed :: Color
paleVioletRed          = Color 219 112 147 
maroon :: Color
maroon                 = Color 176  48  96 
medium_violet_red :: Color
medium_violet_red      = Color 199  21 133 
mediumVioletRed :: Color
mediumVioletRed        = Color 199  21 133 
violet_red :: Color
violet_red             = Color 208  32 144 
violetRed :: Color
violetRed              = Color 208  32 144 
magenta :: Color
magenta                = Color 255   0 255 
violet :: Color
violet                 = Color 238 130 238 
plum :: Color
plum                   = Color 221 160 221 
orchid :: Color
orchid                 = Color 218 112 214 
medium_orchid :: Color
medium_orchid          = Color 186  85 211 
mediumOrchid :: Color
mediumOrchid           = Color 186  85 211 
dark_orchid :: Color
dark_orchid            = Color 153  50 204 
darkOrchid :: Color
darkOrchid             = Color 153  50 204 
dark_violet :: Color
dark_violet            = Color 148   0 211 
darkViolet :: Color
darkViolet             = Color 148   0 211 
blue_violet :: Color
blue_violet            = Color 138  43 226 
blueViolet :: Color
blueViolet             = Color 138  43 226 
purple :: Color
purple                 = Color 160  32 240 
medium_purple :: Color
medium_purple          = Color 147 112 219 
mediumPurple :: Color
mediumPurple           = Color 147 112 219 
thistle :: Color
thistle                = Color 216 191 216 
snow1 :: Color
snow1                  = Color 255 250 250 
snow2 :: Color
snow2                  = Color 238 233 233 
snow3 :: Color
snow3                  = Color 205 201 201 
snow4 :: Color
snow4                  = Color 139 137 137 
seashell1 :: Color
seashell1              = Color 255 245 238 
seashell2 :: Color
seashell2              = Color 238 229 222 
seashell3 :: Color
seashell3              = Color 205 197 191 
seashell4 :: Color
seashell4              = Color 139 134 130 
antiqueWhite1 :: Color
antiqueWhite1          = Color 255 239 219 
antiqueWhite2 :: Color
antiqueWhite2          = Color 238 223 204 
antiqueWhite3 :: Color
antiqueWhite3          = Color 205 192 176 
antiqueWhite4 :: Color
antiqueWhite4          = Color 139 131 120 
bisque1 :: Color
bisque1                = Color 255 228 196 
bisque2 :: Color
bisque2                = Color 238 213 183 
bisque3 :: Color
bisque3                = Color 205 183 158 
bisque4 :: Color
bisque4                = Color 139 125 107 
peachPuff1 :: Color
peachPuff1             = Color 255 218 185 
peachPuff2 :: Color
peachPuff2             = Color 238 203 173 
peachPuff3 :: Color
peachPuff3             = Color 205 175 149 
peachPuff4 :: Color
peachPuff4             = Color 139 119 101 
navajoWhite1 :: Color
navajoWhite1           = Color 255 222 173 
navajoWhite2 :: Color
navajoWhite2           = Color 238 207 161 
navajoWhite3 :: Color
navajoWhite3           = Color 205 179 139 
navajoWhite4 :: Color
navajoWhite4           = Color 139 121  94 
lemonChiffon1 :: Color
lemonChiffon1          = Color 255 250 205 
lemonChiffon2 :: Color
lemonChiffon2          = Color 238 233 191 
lemonChiffon3 :: Color
lemonChiffon3          = Color 205 201 165 
lemonChiffon4 :: Color
lemonChiffon4          = Color 139 137 112 
cornsilk1 :: Color
cornsilk1              = Color 255 248 220 
cornsilk2 :: Color
cornsilk2              = Color 238 232 205 
cornsilk3 :: Color
cornsilk3              = Color 205 200 177 
cornsilk4 :: Color
cornsilk4              = Color 139 136 120 
ivory1 :: Color
ivory1                 = Color 255 255 240 
ivory2 :: Color
ivory2                 = Color 238 238 224 
ivory3 :: Color
ivory3                 = Color 205 205 193 
ivory4 :: Color
ivory4                 = Color 139 139 131 
honeydew1 :: Color
honeydew1              = Color 240 255 240 
honeydew2 :: Color
honeydew2              = Color 224 238 224 
honeydew3 :: Color
honeydew3              = Color 193 205 193 
honeydew4 :: Color
honeydew4              = Color 131 139 131 
lavenderBlush1 :: Color
lavenderBlush1         = Color 255 240 245 
lavenderBlush2 :: Color
lavenderBlush2         = Color 238 224 229 
lavenderBlush3 :: Color
lavenderBlush3         = Color 205 193 197 
lavenderBlush4 :: Color
lavenderBlush4         = Color 139 131 134 
mistyRose1 :: Color
mistyRose1             = Color 255 228 225 
mistyRose2 :: Color
mistyRose2             = Color 238 213 210 
mistyRose3 :: Color
mistyRose3             = Color 205 183 181 
mistyRose4 :: Color
mistyRose4             = Color 139 125 123 
azure1 :: Color
azure1                 = Color 240 255 255 
azure2 :: Color
azure2                 = Color 224 238 238 
azure3 :: Color
azure3                 = Color 193 205 205 
azure4 :: Color
azure4                 = Color 131 139 139 
slateBlue1 :: Color
slateBlue1             = Color 131 111 255 
slateBlue2 :: Color
slateBlue2             = Color 122 103 238 
slateBlue3 :: Color
slateBlue3             = Color 105  89 205 
slateBlue4 :: Color
slateBlue4             = Color  71  60 139 
royalBlue1 :: Color
royalBlue1             = Color  72 118 255 
royalBlue2 :: Color
royalBlue2             = Color  67 110 238 
royalBlue3 :: Color
royalBlue3             = Color  58  95 205 
royalBlue4 :: Color
royalBlue4             = Color  39  64 139 
blue1 :: Color
blue1                  = Color   0   0 255 
blue2 :: Color
blue2                  = Color   0   0 238 
blue3 :: Color
blue3                  = Color   0   0 205 
blue4 :: Color
blue4                  = Color   0   0 139 
dodgerBlue1 :: Color
dodgerBlue1            = Color  30 144 255 
dodgerBlue2 :: Color
dodgerBlue2            = Color  28 134 238 
dodgerBlue3 :: Color
dodgerBlue3            = Color  24 116 205 
dodgerBlue4 :: Color
dodgerBlue4            = Color  16  78 139 
steelBlue1 :: Color
steelBlue1             = Color  99 184 255 
steelBlue2 :: Color
steelBlue2             = Color  92 172 238 
steelBlue3 :: Color
steelBlue3             = Color  79 148 205 
steelBlue4 :: Color
steelBlue4             = Color  54 100 139 
deepSkyBlue1 :: Color
deepSkyBlue1           = Color   0 191 255 
deepSkyBlue2 :: Color
deepSkyBlue2           = Color   0 178 238 
deepSkyBlue3 :: Color
deepSkyBlue3           = Color   0 154 205 
deepSkyBlue4 :: Color
deepSkyBlue4           = Color   0 104 139 
skyBlue1 :: Color
skyBlue1               = Color 135 206 255 
skyBlue2 :: Color
skyBlue2               = Color 126 192 238 
skyBlue3 :: Color
skyBlue3               = Color 108 166 205 
skyBlue4 :: Color
skyBlue4               = Color  74 112 139 
lightSkyBlue1 :: Color
lightSkyBlue1          = Color 176 226 255 
lightSkyBlue2 :: Color
lightSkyBlue2          = Color 164 211 238 
lightSkyBlue3 :: Color
lightSkyBlue3          = Color 141 182 205 
lightSkyBlue4 :: Color
lightSkyBlue4          = Color  96 123 139 
slateGray1 :: Color
slateGray1             = Color 198 226 255 
slateGray2 :: Color
slateGray2             = Color 185 211 238 
slateGray3 :: Color
slateGray3             = Color 159 182 205 
slateGray4 :: Color
slateGray4             = Color 108 123 139 
lightSteelBlue1 :: Color
lightSteelBlue1        = Color 202 225 255 
lightSteelBlue2 :: Color
lightSteelBlue2        = Color 188 210 238 
lightSteelBlue3 :: Color
lightSteelBlue3        = Color 162 181 205 
lightSteelBlue4 :: Color
lightSteelBlue4        = Color 110 123 139 
lightBlue1 :: Color
lightBlue1             = Color 191 239 255 
lightBlue2 :: Color
lightBlue2             = Color 178 223 238 
lightBlue3 :: Color
lightBlue3             = Color 154 192 205 
lightBlue4 :: Color
lightBlue4             = Color 104 131 139 
lightCyan1 :: Color
lightCyan1             = Color 224 255 255 
lightCyan2 :: Color
lightCyan2             = Color 209 238 238 
lightCyan3 :: Color
lightCyan3             = Color 180 205 205 
lightCyan4 :: Color
lightCyan4             = Color 122 139 139 
paleTurquoise1 :: Color
paleTurquoise1         = Color 187 255 255 
paleTurquoise2 :: Color
paleTurquoise2         = Color 174 238 238 
paleTurquoise3 :: Color
paleTurquoise3         = Color 150 205 205 
paleTurquoise4 :: Color
paleTurquoise4         = Color 102 139 139 
cadetBlue1 :: Color
cadetBlue1             = Color 152 245 255 
cadetBlue2 :: Color
cadetBlue2             = Color 142 229 238 
cadetBlue3 :: Color
cadetBlue3             = Color 122 197 205 
cadetBlue4 :: Color
cadetBlue4             = Color  83 134 139 
turquoise1 :: Color
turquoise1             = Color   0 245 255 
turquoise2 :: Color
turquoise2             = Color   0 229 238 
turquoise3 :: Color
turquoise3             = Color   0 197 205 
turquoise4 :: Color
turquoise4             = Color   0 134 139 
cyan1 :: Color
cyan1                  = Color   0 255 255 
cyan2 :: Color
cyan2                  = Color   0 238 238 
cyan3 :: Color
cyan3                  = Color   0 205 205 
cyan4 :: Color
cyan4                  = Color   0 139 139 
darkSlateGray1 :: Color
darkSlateGray1         = Color 151 255 255 
darkSlateGray2 :: Color
darkSlateGray2         = Color 141 238 238 
darkSlateGray3 :: Color
darkSlateGray3         = Color 121 205 205 
darkSlateGray4 :: Color
darkSlateGray4         = Color  82 139 139 
aquamarine1 :: Color
aquamarine1            = Color 127 255 212 
aquamarine2 :: Color
aquamarine2            = Color 118 238 198 
aquamarine3 :: Color
aquamarine3            = Color 102 205 170 
aquamarine4 :: Color
aquamarine4            = Color  69 139 116 
darkSeaGreen1 :: Color
darkSeaGreen1          = Color 193 255 193 
darkSeaGreen2 :: Color
darkSeaGreen2          = Color 180 238 180 
darkSeaGreen3 :: Color
darkSeaGreen3          = Color 155 205 155 
darkSeaGreen4 :: Color
darkSeaGreen4          = Color 105 139 105 
seaGreen1 :: Color
seaGreen1              = Color  84 255 159 
seaGreen2 :: Color
seaGreen2              = Color  78 238 148 
seaGreen3 :: Color
seaGreen3              = Color  67 205 128 
seaGreen4 :: Color
seaGreen4              = Color  46 139  87 
paleGreen1 :: Color
paleGreen1             = Color 154 255 154 
paleGreen2 :: Color
paleGreen2             = Color 144 238 144 
paleGreen3 :: Color
paleGreen3             = Color 124 205 124 
paleGreen4 :: Color
paleGreen4             = Color  84 139  84 
springGreen1 :: Color
springGreen1           = Color   0 255 127 
springGreen2 :: Color
springGreen2           = Color   0 238 118 
springGreen3 :: Color
springGreen3           = Color   0 205 102 
springGreen4 :: Color
springGreen4           = Color   0 139  69 
green1 :: Color
green1                 = Color   0 255   0 
green2 :: Color
green2                 = Color   0 238   0 
green3 :: Color
green3                 = Color   0 205   0 
green4 :: Color
green4                 = Color   0 139   0 
chartreuse1 :: Color
chartreuse1            = Color 127 255   0 
chartreuse2 :: Color
chartreuse2            = Color 118 238   0 
chartreuse3 :: Color
chartreuse3            = Color 102 205   0 
chartreuse4 :: Color
chartreuse4            = Color  69 139   0 
oliveDrab1 :: Color
oliveDrab1             = Color 192 255  62 
oliveDrab2 :: Color
oliveDrab2             = Color 179 238  58 
oliveDrab3 :: Color
oliveDrab3             = Color 154 205  50 
oliveDrab4 :: Color
oliveDrab4             = Color 105 139  34 
darkOliveGreen1 :: Color
darkOliveGreen1        = Color 202 255 112 
darkOliveGreen2 :: Color
darkOliveGreen2        = Color 188 238 104 
darkOliveGreen3 :: Color
darkOliveGreen3        = Color 162 205  90 
darkOliveGreen4 :: Color
darkOliveGreen4        = Color 110 139  61 
khaki1 :: Color
khaki1                 = Color 255 246 143 
khaki2 :: Color
khaki2                 = Color 238 230 133 
khaki3 :: Color
khaki3                 = Color 205 198 115 
khaki4 :: Color
khaki4                 = Color 139 134  78 
lightGoldenrod1 :: Color
lightGoldenrod1        = Color 255 236 139 
lightGoldenrod2 :: Color
lightGoldenrod2        = Color 238 220 130 
lightGoldenrod3 :: Color
lightGoldenrod3        = Color 205 190 112 
lightGoldenrod4 :: Color
lightGoldenrod4        = Color 139 129  76 
lightYellow1 :: Color
lightYellow1           = Color 255 255 224 
lightYellow2 :: Color
lightYellow2           = Color 238 238 209 
lightYellow3 :: Color
lightYellow3           = Color 205 205 180 
lightYellow4 :: Color
lightYellow4           = Color 139 139 122 
yellow1 :: Color
yellow1                = Color 255 255   0 
yellow2 :: Color
yellow2                = Color 238 238   0 
yellow3 :: Color
yellow3                = Color 205 205   0 
yellow4 :: Color
yellow4                = Color 139 139   0 
gold1 :: Color
gold1                  = Color 255 215   0 
gold2 :: Color
gold2                  = Color 238 201   0 
gold3 :: Color
gold3                  = Color 205 173   0 
gold4 :: Color
gold4                  = Color 139 117   0 
goldenrod1 :: Color
goldenrod1             = Color 255 193  37 
goldenrod2 :: Color
goldenrod2             = Color 238 180  34 
goldenrod3 :: Color
goldenrod3             = Color 205 155  29 
goldenrod4 :: Color
goldenrod4             = Color 139 105  20 
darkGoldenrod1 :: Color
darkGoldenrod1         = Color 255 185  15 
darkGoldenrod2 :: Color
darkGoldenrod2         = Color 238 173  14 
darkGoldenrod3 :: Color
darkGoldenrod3         = Color 205 149  12 
darkGoldenrod4 :: Color
darkGoldenrod4         = Color 139 101   8 
rosyBrown1 :: Color
rosyBrown1             = Color 255 193 193 
rosyBrown2 :: Color
rosyBrown2             = Color 238 180 180 
rosyBrown3 :: Color
rosyBrown3             = Color 205 155 155 
rosyBrown4 :: Color
rosyBrown4             = Color 139 105 105 
indianRed1 :: Color
indianRed1             = Color 255 106 106 
indianRed2 :: Color
indianRed2             = Color 238  99  99 
indianRed3 :: Color
indianRed3             = Color 205  85  85 
indianRed4 :: Color
indianRed4             = Color 139  58  58 
sienna1 :: Color
sienna1                = Color 255 130  71 
sienna2 :: Color
sienna2                = Color 238 121  66 
sienna3 :: Color
sienna3                = Color 205 104  57 
sienna4 :: Color
sienna4                = Color 139  71  38 
burlywood1 :: Color
burlywood1             = Color 255 211 155 
burlywood2 :: Color
burlywood2             = Color 238 197 145 
burlywood3 :: Color
burlywood3             = Color 205 170 125 
burlywood4 :: Color
burlywood4             = Color 139 115  85 
wheat1 :: Color
wheat1                 = Color 255 231 186 
wheat2 :: Color
wheat2                 = Color 238 216 174 
wheat3 :: Color
wheat3                 = Color 205 186 150 
wheat4 :: Color
wheat4                 = Color 139 126 102 
tan1 :: Color
tan1                   = Color 255 165  79 
tan2 :: Color
tan2                   = Color 238 154  73 
tan3 :: Color
tan3                   = Color 205 133  63 
tan4 :: Color
tan4                   = Color 139  90  43 
chocolate1 :: Color
chocolate1             = Color 255 127  36 
chocolate2 :: Color
chocolate2             = Color 238 118  33 
chocolate3 :: Color
chocolate3             = Color 205 102  29 
chocolate4 :: Color
chocolate4             = Color 139  69  19 
firebrick1 :: Color
firebrick1             = Color 255  48  48 
firebrick2 :: Color
firebrick2             = Color 238  44  44 
firebrick3 :: Color
firebrick3             = Color 205  38  38 
firebrick4 :: Color
firebrick4             = Color 139  26  26 
brown1 :: Color
brown1                 = Color 255  64  64 
brown2 :: Color
brown2                 = Color 238  59  59 
brown3 :: Color
brown3                 = Color 205  51  51 
brown4 :: Color
brown4                 = Color 139  35  35 
salmon1 :: Color
salmon1                = Color 255 140 105 
salmon2 :: Color
salmon2                = Color 238 130  98 
salmon3 :: Color
salmon3                = Color 205 112  84 
salmon4 :: Color
salmon4                = Color 139  76  57 
lightSalmon1 :: Color
lightSalmon1           = Color 255 160 122 
lightSalmon2 :: Color
lightSalmon2           = Color 238 149 114 
lightSalmon3 :: Color
lightSalmon3           = Color 205 129  98 
lightSalmon4 :: Color
lightSalmon4           = Color 139  87  66 
orange1 :: Color
orange1                = Color 255 165   0 
orange2 :: Color
orange2                = Color 238 154   0 
orange3 :: Color
orange3                = Color 205 133   0 
orange4 :: Color
orange4                = Color 139  90   0 
darkOrange1 :: Color
darkOrange1            = Color 255 127   0 
darkOrange2 :: Color
darkOrange2            = Color 238 118   0 
darkOrange3 :: Color
darkOrange3            = Color 205 102   0 
darkOrange4 :: Color
darkOrange4            = Color 139  69   0 
coral1 :: Color
coral1                 = Color 255 114  86 
coral2 :: Color
coral2                 = Color 238 106  80 
coral3 :: Color
coral3                 = Color 205  91  69 
coral4 :: Color
coral4                 = Color 139  62  47 
tomato1 :: Color
tomato1                = Color 255  99  71 
tomato2 :: Color
tomato2                = Color 238  92  66 
tomato3 :: Color
tomato3                = Color 205  79  57 
tomato4 :: Color
tomato4                = Color 139  54  38 
orangeRed1 :: Color
orangeRed1             = Color 255  69   0 
orangeRed2 :: Color
orangeRed2             = Color 238  64   0 
orangeRed3 :: Color
orangeRed3             = Color 205  55   0 
orangeRed4 :: Color
orangeRed4             = Color 139  37   0 
red1 :: Color
red1                   = Color 255   0   0 
red2 :: Color
red2                   = Color 238   0   0 
red3 :: Color
red3                   = Color 205   0   0 
red4 :: Color
red4                   = Color 139   0   0 
deepPink1 :: Color
deepPink1              = Color 255  20 147 
deepPink2 :: Color
deepPink2              = Color 238  18 137 
deepPink3 :: Color
deepPink3              = Color 205  16 118 
deepPink4 :: Color
deepPink4              = Color 139  10  80 
hotPink1 :: Color
hotPink1               = Color 255 110 180 
hotPink2 :: Color
hotPink2               = Color 238 106 167 
hotPink3 :: Color
hotPink3               = Color 205  96 144 
hotPink4 :: Color
hotPink4               = Color 139  58  98 
pink1 :: Color
pink1                  = Color 255 181 197 
pink2 :: Color
pink2                  = Color 238 169 184 
pink3 :: Color
pink3                  = Color 205 145 158 
pink4 :: Color
pink4                  = Color 139  99 108 
lightPink1 :: Color
lightPink1             = Color 255 174 185 
lightPink2 :: Color
lightPink2             = Color 238 162 173 
lightPink3 :: Color
lightPink3             = Color 205 140 149 
lightPink4 :: Color
lightPink4             = Color 139  95 101 
paleVioletRed1 :: Color
paleVioletRed1         = Color 255 130 171 
paleVioletRed2 :: Color
paleVioletRed2         = Color 238 121 159 
paleVioletRed3 :: Color
paleVioletRed3         = Color 205 104 137 
paleVioletRed4 :: Color
paleVioletRed4         = Color 139  71  93 
maroon1 :: Color
maroon1                = Color 255  52 179 
maroon2 :: Color
maroon2                = Color 238  48 167 
maroon3 :: Color
maroon3                = Color 205  41 144 
maroon4 :: Color
maroon4                = Color 139  28  98 
violetRed1 :: Color
violetRed1             = Color 255  62 150 
violetRed2 :: Color
violetRed2             = Color 238  58 140 
violetRed3 :: Color
violetRed3             = Color 205  50 120 
violetRed4 :: Color
violetRed4             = Color 139  34  82 
magenta1 :: Color
magenta1               = Color 255   0 255 
magenta2 :: Color
magenta2               = Color 238   0 238 
magenta3 :: Color
magenta3               = Color 205   0 205 
magenta4 :: Color
magenta4               = Color 139   0 139 
orchid1 :: Color
orchid1                = Color 255 131 250 
orchid2 :: Color
orchid2                = Color 238 122 233 
orchid3 :: Color
orchid3                = Color 205 105 201 
orchid4 :: Color
orchid4                = Color 139  71 137 
plum1 :: Color
plum1                  = Color 255 187 255 
plum2 :: Color
plum2                  = Color 238 174 238 
plum3 :: Color
plum3                  = Color 205 150 205 
plum4 :: Color
plum4                  = Color 139 102 139 
mediumOrchid1 :: Color
mediumOrchid1          = Color 224 102 255 
mediumOrchid2 :: Color
mediumOrchid2          = Color 209  95 238 
mediumOrchid3 :: Color
mediumOrchid3          = Color 180  82 205 
mediumOrchid4 :: Color
mediumOrchid4          = Color 122  55 139 
darkOrchid1 :: Color
darkOrchid1            = Color 191  62 255 
darkOrchid2 :: Color
darkOrchid2            = Color 178  58 238 
darkOrchid3 :: Color
darkOrchid3            = Color 154  50 205 
darkOrchid4 :: Color
darkOrchid4            = Color 104  34 139 
purple1 :: Color
purple1                = Color 155  48 255 
purple2 :: Color
purple2                = Color 145  44 238 
purple3 :: Color
purple3                = Color 125  38 205 
purple4 :: Color
purple4                = Color  85  26 139 
mediumPurple1 :: Color
mediumPurple1          = Color 171 130 255 
mediumPurple2 :: Color
mediumPurple2          = Color 159 121 238 
mediumPurple3 :: Color
mediumPurple3          = Color 137 104 205 
mediumPurple4 :: Color
mediumPurple4          = Color  93  71 139 
thistle1 :: Color
thistle1               = Color 255 225 255 
thistle2 :: Color
thistle2               = Color 238 210 238 
thistle3 :: Color
thistle3               = Color 205 181 205 
thistle4 :: Color
thistle4               = Color 139 123 139 
gray0 :: Color
gray0                  = Color   0   0   0 
grey0 :: Color
grey0                  = Color   0   0   0 
gray1 :: Color
gray1                  = Color   3   3   3 
grey1 :: Color
grey1                  = Color   3   3   3 
gray2 :: Color
gray2                  = Color   5   5   5 
grey2 :: Color
grey2                  = Color   5   5   5 
gray3 :: Color
gray3                  = Color   8   8   8 
grey3 :: Color
grey3                  = Color   8   8   8 
gray4 :: Color
gray4                  = Color  10  10  10 
grey4 :: Color
grey4                  = Color  10  10  10 
gray5 :: Color
gray5                  = Color  13  13  13 
grey5 :: Color
grey5                  = Color  13  13  13 
gray6 :: Color
gray6                  = Color  15  15  15 
grey6 :: Color
grey6                  = Color  15  15  15 
gray7 :: Color
gray7                  = Color  18  18  18 
grey7 :: Color
grey7                  = Color  18  18  18 
gray8 :: Color
gray8                  = Color  20  20  20 
grey8 :: Color
grey8                  = Color  20  20  20 
gray9 :: Color
gray9                  = Color  23  23  23 
grey9 :: Color
grey9                  = Color  23  23  23 
gray10 :: Color
gray10                 = Color  26  26  26 
grey10 :: Color
grey10                 = Color  26  26  26 
gray11 :: Color
gray11                 = Color  28  28  28 
grey11 :: Color
grey11                 = Color  28  28  28 
gray12 :: Color
gray12                 = Color  31  31  31 
grey12 :: Color
grey12                 = Color  31  31  31 
gray13 :: Color
gray13                 = Color  33  33  33 
grey13 :: Color
grey13                 = Color  33  33  33 
gray14 :: Color
gray14                 = Color  36  36  36 
grey14 :: Color
grey14                 = Color  36  36  36 
gray15 :: Color
gray15                 = Color  38  38  38 
grey15 :: Color
grey15                 = Color  38  38  38 
gray16 :: Color
gray16                 = Color  41  41  41 
grey16 :: Color
grey16                 = Color  41  41  41 
gray17 :: Color
gray17                 = Color  43  43  43 
grey17 :: Color
grey17                 = Color  43  43  43 
gray18 :: Color
gray18                 = Color  46  46  46 
grey18 :: Color
grey18                 = Color  46  46  46 
gray19 :: Color
gray19                 = Color  48  48  48 
grey19 :: Color
grey19                 = Color  48  48  48 
gray20 :: Color
gray20                 = Color  51  51  51 
grey20 :: Color
grey20                 = Color  51  51  51 
gray21 :: Color
gray21                 = Color  54  54  54 
grey21 :: Color
grey21                 = Color  54  54  54 
gray22 :: Color
gray22                 = Color  56  56  56 
grey22 :: Color
grey22                 = Color  56  56  56 
gray23 :: Color
gray23                 = Color  59  59  59 
grey23 :: Color
grey23                 = Color  59  59  59 
gray24 :: Color
gray24                 = Color  61  61  61 
grey24 :: Color
grey24                 = Color  61  61  61 
gray25 :: Color
gray25                 = Color  64  64  64 
grey25 :: Color
grey25                 = Color  64  64  64 
gray26 :: Color
gray26                 = Color  66  66  66 
grey26 :: Color
grey26                 = Color  66  66  66 
gray27 :: Color
gray27                 = Color  69  69  69 
grey27 :: Color
grey27                 = Color  69  69  69 
gray28 :: Color
gray28                 = Color  71  71  71 
grey28 :: Color
grey28                 = Color  71  71  71 
gray29 :: Color
gray29                 = Color  74  74  74 
grey29 :: Color
grey29                 = Color  74  74  74 
gray30 :: Color
gray30                 = Color  77  77  77 
grey30 :: Color
grey30                 = Color  77  77  77 
gray31 :: Color
gray31                 = Color  79  79  79 
grey31 :: Color
grey31                 = Color  79  79  79 
gray32 :: Color
gray32                 = Color  82  82  82 
grey32 :: Color
grey32                 = Color  82  82  82 
gray33 :: Color
gray33                 = Color  84  84  84 
grey33 :: Color
grey33                 = Color  84  84  84 
gray34 :: Color
gray34                 = Color  87  87  87 
grey34 :: Color
grey34                 = Color  87  87  87 
gray35 :: Color
gray35                 = Color  89  89  89 
grey35 :: Color
grey35                 = Color  89  89  89 
gray36 :: Color
gray36                 = Color  92  92  92 
grey36 :: Color
grey36                 = Color  92  92  92 
gray37 :: Color
gray37                 = Color  94  94  94 
grey37 :: Color
grey37                 = Color  94  94  94 
gray38 :: Color
gray38                 = Color  97  97  97 
grey38 :: Color
grey38                 = Color  97  97  97 
gray39 :: Color
gray39                 = Color  99  99  99 
grey39 :: Color
grey39                 = Color  99  99  99 
gray40 :: Color
gray40                 = Color 102 102 102 
grey40 :: Color
grey40                 = Color 102 102 102 
gray41 :: Color
gray41                 = Color 105 105 105 
grey41 :: Color
grey41                 = Color 105 105 105 
gray42 :: Color
gray42                 = Color 107 107 107 
grey42 :: Color
grey42                 = Color 107 107 107 
gray43 :: Color
gray43                 = Color 110 110 110 
grey43 :: Color
grey43                 = Color 110 110 110 
gray44 :: Color
gray44                 = Color 112 112 112 
grey44 :: Color
grey44                 = Color 112 112 112 
gray45 :: Color
gray45                 = Color 115 115 115 
grey45 :: Color
grey45                 = Color 115 115 115 
gray46 :: Color
gray46                 = Color 117 117 117 
grey46 :: Color
grey46                 = Color 117 117 117 
gray47 :: Color
gray47                 = Color 120 120 120 
grey47 :: Color
grey47                 = Color 120 120 120 
gray48 :: Color
gray48                 = Color 122 122 122 
grey48 :: Color
grey48                 = Color 122 122 122 
gray49 :: Color
gray49                 = Color 125 125 125 
grey49 :: Color
grey49                 = Color 125 125 125 
gray50 :: Color
gray50                 = Color 127 127 127 
grey50 :: Color
grey50                 = Color 127 127 127 
gray51 :: Color
gray51                 = Color 130 130 130 
grey51 :: Color
grey51                 = Color 130 130 130 
gray52 :: Color
gray52                 = Color 133 133 133 
grey52 :: Color
grey52                 = Color 133 133 133 
gray53 :: Color
gray53                 = Color 135 135 135 
grey53 :: Color
grey53                 = Color 135 135 135 
gray54 :: Color
gray54                 = Color 138 138 138 
grey54 :: Color
grey54                 = Color 138 138 138 
gray55 :: Color
gray55                 = Color 140 140 140 
grey55 :: Color
grey55                 = Color 140 140 140 
gray56 :: Color
gray56                 = Color 143 143 143 
grey56 :: Color
grey56                 = Color 143 143 143 
gray57 :: Color
gray57                 = Color 145 145 145 
grey57 :: Color
grey57                 = Color 145 145 145 
gray58 :: Color
gray58                 = Color 148 148 148 
grey58 :: Color
grey58                 = Color 148 148 148 
gray59 :: Color
gray59                 = Color 150 150 150 
grey59 :: Color
grey59                 = Color 150 150 150 
gray60 :: Color
gray60                 = Color 153 153 153 
grey60 :: Color
grey60                 = Color 153 153 153 
gray61 :: Color
gray61                 = Color 156 156 156 
grey61 :: Color
grey61                 = Color 156 156 156 
gray62 :: Color
gray62                 = Color 158 158 158 
grey62 :: Color
grey62                 = Color 158 158 158 
gray63 :: Color
gray63                 = Color 161 161 161 
grey63 :: Color
grey63                 = Color 161 161 161 
gray64 :: Color
gray64                 = Color 163 163 163 
grey64 :: Color
grey64                 = Color 163 163 163 
gray65 :: Color
gray65                 = Color 166 166 166 
grey65 :: Color
grey65                 = Color 166 166 166 
gray66 :: Color
gray66                 = Color 168 168 168 
grey66 :: Color
grey66                 = Color 168 168 168 
gray67 :: Color
gray67                 = Color 171 171 171 
grey67 :: Color
grey67                 = Color 171 171 171 
gray68 :: Color
gray68                 = Color 173 173 173 
grey68 :: Color
grey68                 = Color 173 173 173 
gray69 :: Color
gray69                 = Color 176 176 176 
grey69 :: Color
grey69                 = Color 176 176 176 
gray70 :: Color
gray70                 = Color 179 179 179 
grey70 :: Color
grey70                 = Color 179 179 179 
gray71 :: Color
gray71                 = Color 181 181 181 
grey71 :: Color
grey71                 = Color 181 181 181 
gray72 :: Color
gray72                 = Color 184 184 184 
grey72 :: Color
grey72                 = Color 184 184 184 
gray73 :: Color
gray73                 = Color 186 186 186 
grey73 :: Color
grey73                 = Color 186 186 186 
gray74 :: Color
gray74                 = Color 189 189 189 
grey74 :: Color
grey74                 = Color 189 189 189 
gray75 :: Color
gray75                 = Color 191 191 191 
grey75 :: Color
grey75                 = Color 191 191 191 
gray76 :: Color
gray76                 = Color 194 194 194 
grey76 :: Color
grey76                 = Color 194 194 194 
gray77 :: Color
gray77                 = Color 196 196 196 
grey77 :: Color
grey77                 = Color 196 196 196 
gray78 :: Color
gray78                 = Color 199 199 199 
grey78 :: Color
grey78                 = Color 199 199 199 
gray79 :: Color
gray79                 = Color 201 201 201 
grey79 :: Color
grey79                 = Color 201 201 201 
gray80 :: Color
gray80                 = Color 204 204 204 
grey80 :: Color
grey80                 = Color 204 204 204 
gray81 :: Color
gray81                 = Color 207 207 207 
grey81 :: Color
grey81                 = Color 207 207 207 
gray82 :: Color
gray82                 = Color 209 209 209 
grey82 :: Color
grey82                 = Color 209 209 209 
gray83 :: Color
gray83                 = Color 212 212 212 
grey83 :: Color
grey83                 = Color 212 212 212 
gray84 :: Color
gray84                 = Color 214 214 214 
grey84 :: Color
grey84                 = Color 214 214 214 
gray85 :: Color
gray85                 = Color 217 217 217 
grey85 :: Color
grey85                 = Color 217 217 217 
gray86 :: Color
gray86                 = Color 219 219 219 
grey86 :: Color
grey86                 = Color 219 219 219 
gray87 :: Color
gray87                 = Color 222 222 222 
grey87 :: Color
grey87                 = Color 222 222 222 
gray88 :: Color
gray88                 = Color 224 224 224 
grey88 :: Color
grey88                 = Color 224 224 224 
gray89 :: Color
gray89                 = Color 227 227 227 
grey89 :: Color
grey89                 = Color 227 227 227 
gray90 :: Color
gray90                 = Color 229 229 229 
grey90 :: Color
grey90                 = Color 229 229 229 
gray91 :: Color
gray91                 = Color 232 232 232 
grey91 :: Color
grey91                 = Color 232 232 232 
gray92 :: Color
gray92                 = Color 235 235 235 
grey92 :: Color
grey92                 = Color 235 235 235 
gray93 :: Color
gray93                 = Color 237 237 237 
grey93 :: Color
grey93                 = Color 237 237 237 
gray94 :: Color
gray94                 = Color 240 240 240 
grey94 :: Color
grey94                 = Color 240 240 240 
gray95 :: Color
gray95                 = Color 242 242 242 
grey95 :: Color
grey95                 = Color 242 242 242 
gray96 :: Color
gray96                 = Color 245 245 245 
grey96 :: Color
grey96                 = Color 245 245 245 
gray97 :: Color
gray97                 = Color 247 247 247 
grey97 :: Color
grey97                 = Color 247 247 247 
gray98 :: Color
gray98                 = Color 250 250 250 
grey98 :: Color
grey98                 = Color 250 250 250 
gray99 :: Color
gray99                 = Color 252 252 252 
grey99 :: Color
grey99                 = Color 252 252 252 
gray100 :: Color
gray100                = Color 255 255 255 
grey100 :: Color
grey100                = Color 255 255 255 
dark_grey :: Color
dark_grey              = Color 169 169 169 
darkGrey :: Color
darkGrey               = Color 169 169 169 
dark_gray :: Color
dark_gray              = Color 169 169 169 
darkGray :: Color
darkGray               = Color 169 169 169 
dark_blue :: Color
dark_blue              = Color 0     0 139 
darkBlue :: Color
darkBlue               = Color 0     0 139 
dark_cyan :: Color
dark_cyan              = Color 0   139 139 
darkCyan :: Color
darkCyan               = Color 0   139 139 
dark_magenta :: Color
dark_magenta           = Color 139   0 139 
darkMagenta :: Color
darkMagenta            = Color 139   0 139 
dark_red :: Color
dark_red               = Color 139   0   0 
darkRed :: Color
darkRed                = Color 139   0   0 
light_green :: Color
light_green            = Color 144 238 144 
lightGreen :: Color
lightGreen             = Color 144 238 144 

