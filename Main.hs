{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Either       (fromRight)
import Text.Megaparsec   (parse, parseTest)
import Text.RawString.QQ

import Checker
import Parser
import PrettyPrinter
import Spec

main :: IO ()
main = putStrLn ""

test =
  tail
    [r|
d = "asd"
def a(b):
  c = b+d+e
print(a(c))
|]

parseShow :: PrettyPrint a => Parser a -> String -> IO ()
parseShow prs str = do
  let (Right ast) = parse prs "" str
  putStrLn $ ppshow ast

parseProgram :: String -> Program
parseProgram input =
  case parse program "" input of
    Left _  -> Program []
    Right p -> p

checkInput :: String -> IO ()
checkInput input =
  case check $ checkProgram (parseProgram input) of
    (OK, _)         -> putStrLn $ ppshow "All OK!"
    (NotOK, errors) -> putStrLn "Error" >> putStrLn (ppshow errors)
