module Spec where

  type Symbol = String

  data Expression
    = Var Symbol
    | Num Int
    | Str String
    | Expression :+: Expression
    | Expression :-: Expression
    | Expression :*: Expression
    | Expression :/: Expression
    | Expression :<: Expression
    | Expression :>: Expression
    | FunCall Symbol [Expression]
    | Par Expression
    deriving (Show)

  type Statement = Expression

  data Construct
    = FunDef Symbol [Symbol] [Construct]
    | Stmt Statement
    | Assign Symbol Expression
    | If Expression [Construct] [Construct]
    | While Expression [Construct]
    deriving (Show)

  newtype Program = Program [Construct] deriving (Show)