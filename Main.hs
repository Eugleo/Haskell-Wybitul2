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
if 3 + a > 18: a= 2 else: dont_go_fuck()
|]

parseShow :: PrettyPrint a => Parser a -> String -> IO ()
parseShow prs str = do
  let (Right ast) = parse prs "" str
  putStrLn $ ppshow ast
