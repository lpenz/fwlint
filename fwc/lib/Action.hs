
module Action where

#ifndef PROFILE
import Test.QuickCheck
import Monad
#endif

import qualified Parse
import qualified Doc

------------------------------------------------------------------------------

data Action = Accept | Deny deriving (Eq, Show, Read, Ord)

instance Doc.Able Action

#ifndef PROFILE
instance Arbitrary Action where
	arbitrary = oneof [return Accept, return Deny]
	coarbitrary = undefined
#endif

instance Parse.Parseable Action where
	parseImpl = Parse.lexeme $ do
		Parse.stringForValue [("Accept", Accept), ("Deny", Deny)]

