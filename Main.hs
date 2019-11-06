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
def displayMsg   (msg,x):
  print(msg + string(x))
i= 0
j = readLine( )
while i<j:
    if even(i) :
      displayMsg("Even: ", i)
    else:
      displayMsg("Not even: ", i)
    i=i+1
|]

def displayMsg(msg, x): {
    print(msg + string(x))
}

i = 0
j = readLine()
while i < j: {
    if even(i): {
        displayMsg("Even: ", i)
    }
    else: {
        displayMsg("Not even: ", i)
    }
    i = i + 1
}

parseShow :: PDoc a => Parser a -> String -> IO ()
parseShow prs str = do
  let (Right ast) = parse prs "" str
  putStrLn $ ppshow ast
