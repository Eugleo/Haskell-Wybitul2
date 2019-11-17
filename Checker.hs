module Checker where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.Lazy
import           Data.Foldable                   (fold)
import qualified Data.Set                        as S

import           Spec

data Status
  = OK
  | NotOK

instance Semigroup Status where
  OK <> OK = OK
  _ <> _ = NotOK

instance Monoid Status where
  mempty = OK
  mappend OK OK = OK
  mappend _ _   = NotOK

data Error =
  Undeclared Symbol
             Context

type Variables = S.Set Symbol

type Context = [Construct]

type Checker = ReaderT (Context, Variables) (Writer [Error]) Status

checkProgram :: Program -> Checker
checkProgram (Program cs) = checkConstructs cs

checkVariableName :: Symbol -> Checker
checkVariableName symbol = do
  (context, vars) <- ask
  let status = ok (symbol `S.member` vars)
  case status of
    OK -> return OK
    NotOK -> do
      lift $ tell [Undeclared symbol context]
      return NotOK
  where
    ok True  = OK
    ok False = NotOK

checkAll :: (a -> Checker) -> [a] -> Checker
checkAll checker = fmap fold . mapM checker

checkAllExp :: [Expression] -> Checker
checkAllExp = checkAll checkExpression

checkExpression :: Expression -> Checker
checkExpression (Var symbol) = checkVariableName symbol
checkExpression (e1 :+: e2) = checkAllExp [e1, e2]
checkExpression (e1 :-: e2) = checkAllExp [e1, e2]
checkExpression (e1 :/: e2) = checkAllExp [e1, e2]
checkExpression (e1 :*: e2) = checkAllExp [e1, e2]
checkExpression (e1 :<: e2) = checkAllExp [e1, e2]
checkExpression (e1 :>: e2) = checkAllExp [e1, e2]
checkExpression (FunCall name args) =
  mappend <$> checkVariableName name <*> checkAllExp args
checkExpression (Par exp) = checkExpression exp
checkExpression _ = pure OK

addToContext :: Construct -> (Context, Variables) -> (Context, Variables)
addToContext c (context, vars) = (c : context, vars)

addToVars :: [Symbol] -> (Context, Variables) -> (Context, Variables)
addToVars vs = fmap (S.union $ S.fromList vs)

collect :: a -> [a]
collect = (: [])

checkConstructs :: [Construct] -> Checker
checkConstructs [] = return OK
checkConstructs (c@(Stmt exp):cs) =
  mappend <$> local (addToContext c) (checkExpression exp) <*>
  checkConstructs cs
checkConstructs (c@(If exp true false):cs) = do
  t1 <- local (addToContext c) (checkExpression exp)
  t2 <- local (addToContext c) (checkConstructs true)
  t3 <- local (addToContext c) (checkConstructs false)
  rest <- checkConstructs cs
  return $ fold [t1, t2, t3, rest]
checkConstructs (c@(While exp body):cs) = do
  t1 <- local (addToContext c) (checkExpression exp)
  t2 <- local (addToContext c) (checkConstructs body)
  rest <- checkConstructs cs
  return $ fold [t1, t2, rest]
checkConstructs (c@(Assign name exp):cs) =
  mappend <$> local (addToContext c) (checkExpression exp) <*>
  local (addToVars [name]) (checkConstructs cs)
checkConstructs (c@(FunDef name args body):cs) =
  mappend <$>
  local (addToVars (name : args) . addToContext c) (checkConstructs body) <*>
  local (addToVars [name]) (checkConstructs cs)

definedConstants :: [Symbol]
definedConstants = ["pass", "print", "read"]

check :: Checker -> (Status, [Error])
check checker = runWriter (runReaderT checker ([], S.fromList definedConstants))
