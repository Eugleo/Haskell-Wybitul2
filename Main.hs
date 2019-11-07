{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Either       (fromRight)
import Text.Megaparsec   (parse, parseTest)
import Text.RawString.QQ

import Parser
import PrettyPrinter
import Spec

main :: IO ()
main = putStrLn ""

test =
  tail
    [r|
def add(a, b): return(a + b) / 20
|]

parseShow :: PrettyPrint a => Parser a -> String -> IO ()
parseShow prs str = do
  let (Right ast) = parse prs "" str
  putStrLn $ ppshow ast
