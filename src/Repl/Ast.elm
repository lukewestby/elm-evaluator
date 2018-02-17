module Repl.Ast exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Expr
    = EUnit
    | EVarLocal Name
    | EVarImported Name Name
    | EChar Char
    | EString String
    | EInt Int
    | EFloat Float
    | EBool Bool
    | ETuple2 Expr Expr
    | EList (List Expr)
    | ENegate Expr
    | ELambda Name Expr
    | ECall Expr Expr
    | ELet ( Name, Expr ) Expr
    | EIf (List ( Expr, Expr )) Expr
    | EKernel Name Expr
    | EClosure (Dict Name Expr) Name Expr


type Ord
    = Lt
    | Lte
    | Gte
    | Gt


type Eq
    = Eq
    | Neq
