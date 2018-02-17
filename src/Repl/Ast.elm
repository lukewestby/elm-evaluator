module Repl.Ast exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Expr
    = -- Variables
      EVarLocal Name
    | EVarImported Name Name
      -- Values
    | EUnit
    | EChar Char
    | EString String
    | EInt Int
    | EFloat Float
    | EBool Bool
      -- Containers
    | ETuple2 Expr Expr
    | ERecord (Dict Name Expr)
    | EList (List Expr)
    | ECtor Name (List Expr)
      -- Callables
    | EBinop Name Name Expr Expr
    | ELambda Name Expr
    | EClosure (Dict Name Expr) Name Expr
      -- Operations
    | ENegate Expr
    | ECall Expr Expr
    | ELet ( Name, Expr ) Expr
    | EIf (List ( Expr, Expr )) Expr
    | EUpdate { record : Expr, replacements : Dict Name Expr }
    | EKernel Name (List Expr)
