module LispVal where

import Control.Monad 
import System.Environment
import Text.ParserCombinators.Parsec  hiding (spaces)
import Text.Parsec.Error
--import Text.Parsec.Prim hiding ( sysUnExpectError , unexpected , mkPT , Parsec , Consumed(..) , Reply(..) , State(..) , parsecMap , parserReturn , parserBind , mergeErrorReply , parserFail , parserZero , parserPlus , (<?>) , (<|>) , label , labels , lookAhead , Stream(..) , tokens , try , token , tokenPrim , tokenPrimEx , many , skipMany , manyAccum , runPT , runP , runParserT , runParser , parse , parseTest , getPosition , getInput , setPosition , setInput , getParserState , setParserState , updateParserState , getState , putState , modifyState , setState , updateState
    --)


symbols :: Parser Char  
symbols = oneOf "!$%&|*+-/:<=?>@^_~#"

intsymbols :: Parser Char  
intsymbols = oneOf "1234567890"

readExpr :: String -> String
readExpr s = case parse parseExpr "lisp" s of 
       Left err -> "no match " ++ show err
       Right value -> "found fucking value"

data Generic = First Int | Second Int


genericEmit :: Int -> Generic 
genericEmit a | a >= 10 = First 0 | a < 10 = Second a


genericCase :: Int -> Int
genericCase a = case genericEmit a of 
       	First err -> err
       	Second pass -> pass

data LispVal = Atom String | List [LispVal] | Num Int |  Number String | String String | Bool Bool | Error String
	     | DottedList [LispVal] LispVal

recGeneric :: Int -> IO ()
recGeneric 0 = print 0
recGeneric a = recGeneric (genericCase (a+1)) >> print a

spaces :: Parser ()
spaces  = skipMany1 space 

-- custom bind op definition 
(<-=) ::  a -> (a  -> a) -> a
a <-= f = f a
(<--) ::  a -> (a  -> m b) -> m b
a <-- f = f a
(<==) ::  m a -> (m a  -> m [a]) -> m [a]
a <== f = f a
-- demonstration
test  ::  Int ->  Int
test a = a
testb  :: Int -> Int
testb b = b

t = test 2  <-= testb

tryl :: String -> () -> Either ParseError Char
tryl s dull =  s <-- runParser symbols dull "nigger" 
-- demo 
tr = tryl "fuck" >> return 2;
-- From the book

parseString :: Parser LispVal 
parseString = do char '"'
	         x <- noneOf "\"" <== many
		 char '"'
		 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbols 
	       rest <- (letter <|> digit <|> symbols) <== many
	       let atom = [first] ++ rest
	       return $ case atom of 
		   	  "#t" -> Bool True
			  "#f" -> Bool False 
			  otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Num . read) $ many1 digit

parseList :: Parser LispVal
parseList  =  sepBy parseExpr spaces <-- liftM List  

parseDottedList :: Parser LispVal
parseDottedList  = do head <- endBy parseExpr spaces 
		      tail <- char '.'  >> spaces >> parseExpr
		      return $ DottedList head tail
-- from the book

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> do char '(' 
							     x<-parseList <|> parseDottedList
							     char ')'
							     return x
 
getListSize :: [a] -> Int -> Int 
getListSize  [] a  = a
getListSize (x:xs) a =  getListSize xs (a+1)

raiseErrorNumber :: String -> LispVal 
raiseErrorNumber s | getListSize s 0 > 4 = Error "To big Number"
		   | otherwise  = Number s

-- My version, my parser

pAtom :: String ->  LispVal 
pAtom atom =  case atom of 
		  "#t" -> Bool True
	          "#f" -> Bool False 
		  otherwise -> case parse symbols "lisp" atom of 
	    	  	Right sym  -> Atom [sym]
			Left err -> case parse intsymbols "lisp" atom of 
					Right isym ->  case raiseErrorNumber atom of 
							Error err -> Error err
							otherwise -> Number atom
					Left sym -> String atom

toStr :: String -> String
toStr val = do x <- case pAtom val of 
		Atom atom -> "atom " ++ atom
		Bool True ->  "True"
		Bool False ->  "False"
		String val -> "normal string: "  ++  val
		Number val -> "Number: "  ++  val
		Error err -> err ++ "-> " ++ val
		otherwise -> "NaN"
 	       return x

-- My version, my parser
