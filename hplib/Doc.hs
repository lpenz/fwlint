
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Doc (Able, repr, name, nestup, punct, stru, strucat, strusep, struhcat, showrepr, showname, showpres, Existential, brace, module Text.PrettyPrint) where

import Prelude
import Text.PrettyPrint
import qualified Data.Set as Set
import qualified Data.Map as Map
--import Data.Generics.SYB.WithClass.Basics
--import Data.Generics.SYB.WithClass.Context

class (Show a) => Able a where
	repr :: a -> Doc
	repr = text . Prelude.show
	name :: a -> Doc
	name = repr
	pres :: a -> Doc
	pres = repr

-- data AbleD a = AbleD { reprD :: a -> Doc, nameD :: a -> Doc }
-- 
-- ableProxy :: Proxy AbleD
-- ableProxy = error "urk"
-- 
-- instance Able t => Sat (AbleD t) where
-- 	dict = AbleD { reprD = repr, nameD = name }
-- 
-- instance (Show t, Data AbleD t) => Able t where
-- 	name t = sep (gmapQ ableProxy (nameD dict) t)
-- 	repr t = sep (gmapQ ableProxy (reprD dict) t)

instance Able Integer
instance Able Int
instance Able Bool
instance Able Char where
	repr = char

instance Able String where
	repr [] = empty
	repr e = text e

nestup :: Doc -> Doc
nestup = nest 4

instance (Able a) => Able [a] where
	repr [] = text "[]"
	repr [e] = brackets $ repr e
	repr es@(_:_) = brackets $ sep $ punctuate (char ',') (map (nestup . repr) es)
	name [] = text "[]"
	name [e] = brackets $ name e
	name es@(_:_) = brackets $ sep $ punctuate (char ',') (map (nestup . name) es)
	pres [] = text "[]"
	pres [e] = brackets $ pres e
	pres es@(_:_) = brackets $ sep $ punctuate (char ',') (map (nestup . pres) es)

instance (Able a) => Able (Maybe a) where
	repr Nothing = repr "Nothing"
	repr (Just a) = repr "Just" <+> repr a
	name Nothing = name "Nothing"
	name (Just a) = name a
	pres Nothing = pres "Nothing"
	pres (Just a) = pres "Just" <+> pres a

instance (Able a, Able b) => Able (a, b) where
	repr (a, b) = parens $ sep $ (punct (repr ", ")) [ repr a, repr b ]
	name (a, b) = parens $ sep $ (punct (repr ", ")) [ name a, name b ]
	pres (a, b) = parens $ sep $ (punct (repr ", ")) [ pres a, pres b ]

instance (Able a, Able b, Able c) => Able (a, b, c) where
	repr (a, b, c) = parens $ sep $ (punct (repr ", ")) [ repr a, repr b, repr c ]
	name (a, b, c) = parens $ sep $ (punct (repr ", ")) [ name a, name b, name c ]
	pres (a, b, c) = parens $ sep $ (punct (repr ", ")) [ pres a, pres b, pres c ]

instance (Able a, Able b, Able c, Able d) => Able (a, b, c, d) where
	repr (a, b, c, d) = parens $ sep $ (punct (repr ", ")) [ repr a, repr b, repr c, repr d ]
	name (a, b, c, d) = parens $ sep $ (punct (repr ", ")) [ name a, name b, name c, name d ]
	pres (a, b, c, d) = parens $ sep $ (punct (repr ", ")) [ pres a, pres b, pres c, pres d ]

instance (Able a, Able b, Able c, Able d, Able e) => Able (a, b, c, d, e) where
	repr (a, b, c, d, e) = parens $ sep $ (punct (repr ", ")) [ repr a, repr b, repr c, repr d, repr e ]
	name (a, b, c, d, e) = parens $ sep $ (punct (repr ", ")) [ name a, name b, name c, name d, name e ]
	pres (a, b, c, d, e) = parens $ sep $ (punct (repr ", ")) [ pres a, pres b, pres c, pres d, pres e ]

instance (Able a, Able b) => Able (Either a b) where
	repr (Left a)  = repr a
	repr (Right b) = repr b
	name (Left a)  = name a
	name (Right b) = name b
	pres (Left a)  = pres a
	pres (Right b) = pres b

instance Able Doc where
	repr = id

punct :: Doc -> [Doc] -> [Doc]
punct p par = if null par
	then []
	else (pontuados ++ [last par])
	where
		pontuados = map pontua (init par)
		pontua d = if isEmpty d then empty else d <> p

brace :: Doc -> Doc
brace l = repr "{" <> l <> repr "}"

strusep :: [Doc] -> Doc
strusep = brace . sep . (punct (repr ", "))

strucat :: [Doc] -> Doc
strucat = brace . cat . (punct (repr ", "))

struhcat :: [Doc] -> Doc
struhcat = brace . hcat . (punct (repr ", "))

stru :: [Doc] -> Doc
stru = strusep

myStyle :: Style
myStyle = Style { mode=PageMode, lineLength = 100, ribbonsPerLine = 1.5}

showrepr :: Able a => a -> String
showrepr = (renderStyle myStyle) . repr

showname :: Able a => a -> String
showname = (renderStyle myStyle) . name

showpres :: Able a => a -> String
showpres = (renderStyle myStyle) . pres

data Existential a = Existential a deriving (Show, Read)

instance Able a => Able (Existential a) where
	repr (Existential a) = repr a

instance Able a => Able (Set.Set a) where
	repr s = hsep [ Doc.repr "Set.fromList", Doc.repr $ Set.toList s ]
	name s = hsep [ Doc.repr "Set.fromList", Doc.name $ Set.toList s ]
	pres s = hsep [ Doc.pres "Set.fromList", Doc.pres $ Set.toList s ]

instance (Able a, Able b) => Able (Map.Map a b) where
	repr s = hsep [ Doc.repr "Map.fromList", Doc.repr $ Map.toList s ]
	name s = hsep [ Doc.name "Map.fromList", Doc.name $ Map.toList s ]
	pres s = hsep [ Doc.pres "Map.fromList", Doc.pres $ Map.toList s ]

