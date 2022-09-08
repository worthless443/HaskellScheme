module Main where

import LispVal
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
	  --(args !! 0) <-- toStr <-- print
	  readExpr (args !! 0) <-- print
