
{-# LANGUAGE RankNTypes #-}

module Parse where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Util

#ifdef DEBUG
import Debug.Trace
#endif

type ParseState = (Map.Map String String, Int)

lexer :: T.TokenParser ParseState
lexer = T.makeTokenParser (L.haskellDef { L.reservedNames = ["rule", "src", "dst", "router"] })

type Parser a = P.CharParser ParseState a

whiteSpace :: P.CharParser ParseState ()
whiteSpace = T.whiteSpace lexer

lexeme :: forall a. P.CharParser ParseState a -> P.CharParser ParseState a
lexeme = T.lexeme lexer

symbol :: String -> P.CharParser ParseState String
symbol = T.symbol lexer

natural :: P.CharParser ParseState Integer
natural = T.natural lexer

parens :: forall a. P.CharParser ParseState a -> P.CharParser ParseState a
parens = T.parens lexer

braces :: forall a. P.CharParser ParseState a -> P.CharParser ParseState a
braces = T.braces lexer

squares :: forall a. P.CharParser ParseState a -> P.CharParser ParseState a
squares = T.squares lexer

semi :: P.CharParser ParseState String
semi = T.semi lexer

colon :: P.CharParser ParseState String
colon = T.colon lexer

comma :: P.CharParser ParseState String
comma = T.comma lexer

identifier :: P.CharParser ParseState String
identifier = T.identifier lexer

reserved :: String -> P.CharParser ParseState ()
reserved = T.reserved lexer

reservedOp :: String -> P.CharParser ParseState ()
reservedOp = T.reservedOp lexer

stringForValue :: [(String, a)] -> Parse.Parser a
stringForValue l = foldl1 folder strtoval
	where
		folder prev next = prev P.<|> next
		strtoval = map ( \ (str, val) -> P.string str >> return val) l

alias :: P.GenParser Char ParseState ()
alias = lexeme $ do
	reserved "alias"
	key <- identifier P.<?> "alias key"
	val <- P.manyTill P.anyChar (P.try P.space)
#ifdef DEBUG
	P.updateState ( \ m -> trace ("key "++show key++" val "++show val) (Map.insertWithKey err key val m))
#else
	P.updateState ( \ (m, i) -> (Map.insertWithKey err key val m, i))
#endif
	where
		err k _ _ = error $ "alias " ++ show k ++ " exists"

#if 0
lazyMany :: Parser a -> SourceName -> String -> [a]
lazyMany    p           filename      contents = lm state0
	where
		Right state0 = parse getParserState filename contents -- get an initial state
		lm state = either (error . show) id (parse p' "" "")
			where
				p' = setParserState state >> choice [eof >> return [],
					do
						x <- p
						state' <- getParserState
						return (x:lm state')]
#endif

------------------------------------------------------------------------------

-- Class:

class Parseable p where
	parseImpl :: P.GenParser Char ParseState p
	parseAliased :: P.GenParser Char ParseState p
	parseAliased = lexeme $ do
		(m, _) <- P.getState
		key <- identifier
		if Map.notMember key m
#ifdef DEBUG
			then trace ("no alias for " ++ key) (fail ("key " ++ show key ++ " not found"))
			else trace ("alias for " ++ key ++ " found: " ++ (m Map.! key)) (return $ runAliasParser $ m Map.! key)
#else
			then fail $ "key " ++ show key ++ " not found"
			else return $ runAliasParser $ m Map.! key
#endif
	parse :: P.GenParser Char ParseState p
	parse = lexeme $ do
		P.skipMany alias
		(m, i) <- P.getState
		P.setState (m, i + 1)
		(P.try parseAliased) P.<|> parseImpl
	parseStruct :: P.GenParser Char ParseState p
	parseStruct = do
		braces $ parse
	runParserGetAliases :: String -> (p, Map.Map String String)
	runParserGetAliases input = case P.runParser parseRetState (Map.empty, 0) "stdin" input of
			Left err -> error $ "Input:\n" ++ show input ++ "\nError:\n" ++ show err
			Right result -> result
			where
				parseRetState = do
					rv <- parse
					(st, _) <- P.getState
					return (rv, Util.invertMap st)
	runParser :: String -> p
	runParser = fst . runParserGetAliases
	runAliasParser :: String -> p
	runAliasParser input = case P.runParser parse (Map.empty, 0) "stdin" input of
			Left err -> error $ "alias input:\n" ++ show input ++ "\nalias error:\n" ++ show err
			Right result -> result

instance Parseable p => Parseable [p] where
	parseImpl = lexeme $ do
		list <- squares $ P.sepEndBy parseStruct comma
		return $ list

instance (Parseable a, Parseable b) => Parseable (a, b) where
	parseImpl = lexeme $ do
		symbol "("
		a <- parseStruct
		symbol ","
		b <- parseStruct
		symbol ")"
		return $ (a, b)

instance (Parseable a, Parseable b) => Parseable (Either a b) where
	parseImpl = lexeme $ do
		(P.try parsea) P.<|> parseb
		where
			parsea = lexeme $ do
				a <- parse
				return $ Left a
			parseb = lexeme $ do
				b <- parse
				return $ Right b

instance Parseable Integer where
	parseImpl = natural

