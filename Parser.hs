{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Monad (guard, void)
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Spec
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == ' ' || x == '\t'

block :: Parser a -> Parser a
block = L.lexeme scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

expression :: Parser Expression
expression = makeExprParser expressionTerm expressionOperators

expressionTerm :: Parser Expression
expressionTerm =
  choice
    [ try functionCall <?> "function call",
      parens expression <?> "parenthesised expression",
      variable <?> "variable name",
      number <?> "number",
      str <?> "string"
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
  [ [ InfixL ((:*:) <$ symbol "*" <?> "arithmetic operator"),
      InfixL ((:/:) <$ symbol "/" <?> "arithmetic operator")
    ],
    [ InfixL ((:+:) <$ symbol "+" <?> "arithmetic operator"),
      InfixL ((:-:) <$ symbol "-" <?> "arithmetic operator")
    ],
    [ InfixL ((:<:) <$ symbol "<" <?> "arithmetic operator"),
      InfixL ((:>:) <$ symbol ">" <?> "arithmetic operator")
    ]
  ]

construct :: Parser Construct
construct =
  choice
    [ try oneLineFunctionDefinition <?> "function definition",
      try functionDefinition <?> "function definition",
      try oneLineWhile,
      try while,
      try assignment,
      try oneLineIf,
      try ifelse,
      statement <?> "expression"
    ]

statement :: Parser Construct
statement = Stmt <$> expression

assignment :: Parser Construct
assignment = do
  varName <- variableName
  symbol "="
  Assign varName <$> expression

functionParameters :: Parser [Symbol]
functionParameters = params <|> (space $> [])
  where
    params = do
      param1 <- try variableName
      paramRest <- many (symbol "," *> variableName)
      return $ param1 : paramRest

functionHeader :: Parser (Symbol, [Symbol])
functionHeader = do
  lexeme $ string "def"
  name <- variableName
  params <- parens functionParameters
  symbol ":"
  return (name, params)

functionDefinition :: Parser Construct
functionDefinition =
  L.indentBlock scn $ do
    (name, params) <- functionHeader
    return $ L.IndentSome Nothing (return . FunDef name params) construct

oneLineFunctionDefinition :: Parser Construct
oneLineFunctionDefinition = do
  (name, params) <- functionHeader
  exp <- expression <?> "single expression"
  return $ FunDef name params [Stmt exp]

blockHeader :: String -> Parser Expression
blockHeader blockName = do
  lexeme $ string blockName
  exp <- expression <?> "single expression"
  symbol ":"
  return exp

while :: Parser Construct
while =
  L.indentBlock scn $ do
    exp <- blockHeader "while"
    return $ L.IndentSome Nothing (return . While exp) construct

oneLineWhile :: Parser Construct
oneLineWhile = do
  exp <- blockHeader "while"
  body <- expression <?> "single expression"
  return $ While exp [Stmt body]

ifelse :: Parser Construct
ifelse = do
  (clause, body) <-
    L.indentBlock scn $ do
      exp <- blockHeader "if"
      return $ L.IndentSome Nothing (return . (exp,)) construct
  elseblock <-
    optional
      $ L.indentBlock scn
      $ do
        lexeme $ string "else"
        symbol ":"
        return $ L.IndentSome Nothing return construct
  return $ If clause body (fromMaybe [] elseblock)

oneLineIf :: Parser Construct
oneLineIf = do
  (clause, body) <- do
    exp <- blockHeader "if"
    body <- construct
    return (exp, body)
  elseblock <-
    optional $ do
      lexeme $ string "else"
      symbol ":"
      (: []) <$> construct
  return $ If clause [body] (fromMaybe [] elseblock)

program :: Parser Program
program = Program <$> many (block (L.nonIndented scn construct))