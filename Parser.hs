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

construct :: Parser Construct
construct =
  choice
    [ try oneLineFunctionDefinition <?> "function definition"
    , try functionDefinition <?> "function definition"
    , try oneLineWhile
    , try while
    , try assignment
    , try oneLineIf
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

inlineConstruct :: Parser Construct
inlineConstruct = Stmt <$> try expression <|> construct

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
  newline
  return $ FunDef name params [Stmt exp]

whileHeader :: Parser Expression
whileHeader = do
  lexeme $ string "while"
  exp <- expression
  symbol ":"
  return exp

-- Add support for single-line while loops
while :: Parser Construct
while =
  L.indentBlock scn $ do
    exp <- whileHeader
    return $ L.IndentSome Nothing (return . While exp) construct

oneLineWhile :: Parser Construct
oneLineWhile = do
  exp <- whileHeader
  body <- expression <?> "single expression"
  newline
  return $ While exp [Stmt body]

ifHeader :: Parser Expression
ifHeader = do
  lexeme $ string "if"
  exp <- expression
  symbol ":"
  return exp

ifelse :: Parser Construct
ifelse = do
  (clause, body) <-
    L.indentBlock scn $ do
      exp <- ifHeader
      return $ L.IndentSome Nothing (return . (exp, )) construct
  elseblock <-
    optional $
    L.indentBlock scn $ do
      lexeme $ string "else"
      symbol ":"
      return $ L.IndentSome Nothing return construct
  return $ If clause body (fromMaybe [] elseblock)

oneLineIf :: Parser Construct
oneLineIf = do
  (clause, body) <-
    do exp <- ifHeader
       body <- inlineConstruct
       return (exp, body)
  elseblock <-
    optional $ do
      lexeme $ string "else"
      symbol ":"
      (: []) <$> inlineConstruct
  case elseblock of
    Just elsebody -> return $ If clause [body] elsebody
    Nothing -> do
      newline
      return $ If clause [body] []

program :: Parser Program
program = Program <$> many (L.nonIndented scn construct) <* eof
