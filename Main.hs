module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Either       (fromRight)
import Text.Megaparsec   (parse, parseTest)
import Text.RawString.QQ
import Control.Monad (when)

import Checker
import Parser hiding (Parser, str)
import PrettyPrinter
import Spec

data Options = Options {
  shouldFormat :: Bool,
  shouldCheck :: Bool,
  file :: String
  }

main :: IO ()
main = do
  opts <- execParser options
  file <- readFile $ file opts
  when (shouldFormat opts) (formatSource file)
  when (shouldCheck opts) (checkSource file)
  where
    options = info (optParser <**> helper)
      ( fullDesc
     <> progDesc "Format or check the source in FILE"
     <> header "slpfmt - a formatter and checker for the Slepýš language" )

optParser :: Parser Options
optParser = Options <$>
  switch
          ( long "format"
         <> short 'f'
         <> help "Whether to format the file and print to stdout" ) <*>

  switch
         ( long "check"
        <> short 'c'
        <> help "Whether to check the usage of undeclared variables" ) <*>
 argument str (metavar "FILE")

parseProgram :: String -> Program
parseProgram input =
  case parse program "" input of
    Left _  -> Program []
    Right p -> p

formatSource :: String -> IO ()
formatSource = putStrLn . ppshow 88 . parseProgram

checkSource :: String -> IO ()
checkSource input =
  case check $ checkProgram (parseProgram input) of
    (OK, _)         -> putStrLn $ ppshow 100 "All OK!"
    (NotOK, errors) -> putStrLn "Error" >> putStrLn (ppshow 100 errors)
