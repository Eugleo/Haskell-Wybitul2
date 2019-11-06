{-# LANGUAGE TupleSections #-}

module Parser where

import           Control.Monad                  (guard, void)
import           Control.Monad.Combinators.Expr
import           Data.Functor                   (($>))
import           Data.Maybe                     (fromMaybe)
import           Data.Void                      (Void)
import           Spec
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

expression :: Parser Expression
expression = makeExprParser expressionTerm expressionOperators

expressionTerm :: Parser Expression
expressionTerm =
  choice
    [ try functionCall <?> "function call"
    , parens expression <?> "parenthesised expression"
    , variable <?> "variable name"
    , number <?> "number"
    , str <?> "string"
    ]

functionCall :: Parser Expression
functionCall = do
  name <- variableName
  args <- parens $ arguments <|> (space $> [])
  return $ FunCall name args
  where
    arguments = do
      arg1 <- try expression
      argRest <- many (symbol "," *> expression)
      return $ arg1 : argRest

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Expression
number = Num <$> lexeme (L.signed sc L.decimal)

str :: Parser Expression
str = Str <$> lexeme stringLiteral
  where
    stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

variable :: Parser Expression
variable = Var <$> variableName

variableName :: Parser Symbol
variableName = lexeme (some $ oneOf validChars)
  where
    validChars = "_" ++ ['a' .. 'z'] ++ ['A' .. 'Z']

expressionOperators :: [[Operator Parser Expression]]
expressionOperators =
  [ [ InfixL ((:*:) <$ symbol "*" <?> "arithmetic operator")
    , InfixL ((:/:) <$ symbol "/" <?> "arithmetic operator")
    ]
  , [ InfixL ((:+:) <$ symbol "+" <?> "arithmetic operator")
    , InfixL ((:-:) <$ symbol "-" <?> "arithmetic operator")
    ]
  , [ InfixL ((:<:) <$ symbol "<" <?> "arithmetic operator")
    , InfixL ((:>:) <$ symbol ">" <?> "arithmetic operator")
    ]
  ]

-- TODO add support for braces
construct :: Parser Construct
construct =
  choice
    [ try functionDefinition <?> "function definition"
    , try while
    , try assignment
    , try ifelse
    , statement <?> "expression"
    ]

statement :: Parser Construct
statement = Stmt <$> expression <* eol

assignment :: Parser Construct
assignment = do
  varName <- variableName
  symbol "="
  exp <- expression
  eol
  return $ Assign varName exp

-- Add support for single-line function definitions
functionDefinition :: Parser Construct
functionDefinition =
  L.indentBlock scn $ do
    lexeme $ string "def"
    name <- variableName
    params <- parens $ parameters <|> (space $> [])
    symbol ":"
    return $ L.IndentMany Nothing (return . FunDef name params) construct
  where
    parameters = do
      param1 <- try variableName
      paramRest <- many (symbol "," *> variableName)
      return $ param1 : paramRest

while :: Parser Construct
while =
  L.indentBlock scn $ do
    lexeme $ string "while"
    exp <- expression
    symbol ":"
    return $ L.IndentMany Nothing (return . While exp) construct

ifelse :: Parser Construct
ifelse = do
  (clause, body) <-
    L.indentBlock scn $ do
      lexeme $ string "if"
      exp <- expression
      symbol ":"
      return $ L.IndentSome Nothing (return . (exp, )) construct
  elseblock <-
    optional $
    L.indentBlock scn $ do
      lexeme $ string "else"
      symbol ":"
      return $ L.IndentSome Nothing return construct
  return $ If clause body (fromMaybe [] elseblock)

program :: Parser Program
program = Program <$> many construct
