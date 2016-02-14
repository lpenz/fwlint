
{-# LANGUAGE FlexibleInstances #-}

module MatchAction (module Match, module Action) where

import Monad
import Match
import Action
import qualified Parse
import qualified Doc

------------------------------------------------------------------------------

instance Doc.Able (Match.Match, Action) where
	repr (m, a) = Doc.stru [ Doc.repr $ init $ tail $ Doc.showrepr m, Doc.repr a ]

instance Parse.Parseable (Match, Action) where
	parseImpl = Parse.lexeme $ do
		match <- Parse.parse
		act <- Parse.parse
		return $ (match, act)

