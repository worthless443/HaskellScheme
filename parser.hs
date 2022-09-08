module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbols :: Parser Char  
symbols = oneOf "!$%&|*+-/:<=?>@^_~#"

intsymbols :: Parser Char  
intsymbols = oneOf "1234567890"

readExpr :: String -> String
readExpr s = case parse symbols "lisp" s of 
       Left err -> "no match"
       Right value -> "found fucking value"

data Generic = First Int | Second Int


genericEmit :: Int -> Generic 
genericEmit a | a >= 10 = First 0 | a < 10 = Second a


genericCase :: Int -> Int
genericCase a = case genericEmit a of 
       	First err -> err
       	Second pass -> pass

data LispVal = Atom String | List [LispVal] | Number String | String String | Bool Bool

recGeneric :: Int -> IO ()
recGeneric 0 = print 0
recGeneric a = recGeneric (genericCase (a+1)) >> print a

spaces :: Parser ()
spaces  = skipMany1 space 

-- custom bind op definition 
(<-=) ::  a -> (a  -> a) -> a
a <-= f = f a
(<==) ::  m a -> (m a  -> m [a]) -> m [a]
a <== f = f a
-- demonstration
test  ::  Int ->  Int
test a = a
testb  :: Int -> Int
testb b = b

t = test 2  <-= testb

parseString :: Parser LispVal 
parseString = do char '"'
	         x <- noneOf "\"" <== many
		 char '"'
		 return $ String x


parseAtom :: String -> Parser LispVal
parseAtom atom = do first <- letter <|> symbols 
	       	    rest <- (letter <|> digit <|> symbols) <== many
	       	    return $ case atom of 
		   	  "#t" -> Bool True
			  "#f" -> Bool False 
			  otherwise -> Atom atom

pAtom :: String ->  LispVal 
pAtom atom =  case atom of 
		  "#t" -> Bool True
	          "#f" -> Bool False 
		  otherwise -> case parse symbols "lisp" atom of 
	    	  	Right sym  -> Atom [sym]
			Left err -> case parse intsymbols "lisp" atom of 
					Right isym -> Number [isym]
					Left sym -> String atom

toStr :: String -> String
toStr val = do x <- case pAtom val of 
		Atom atom -> "atom " ++ atom
		Bool True ->  "True"
		Bool False ->  "False"
		String val -> "normal string: "  ++  val
		Number val -> "Number: "  ++  val
		otherwise -> "NaN"
 	       return x


-- TODO Ok the symbols are defined, now define the grammer and evals

main :: IO ()
main = do args <- getArgs 
	  print (toStr (args !! 0))
