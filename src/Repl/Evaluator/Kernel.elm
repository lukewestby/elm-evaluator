module Repl.Evaluator.Kernel exposing (..)

import Repl.Ast as Ast exposing (Expr(..), Name)


evaluate : Name -> Expr -> Result String Expr
evaluate name inputs =
    case name of
        "_Basics_add" ->
            case inputs of
                ETuple2 (EInt l) (EInt r) ->
                    Ok <| EInt (l + r)

                ETuple2 (EFloat l) (EFloat r) ->
                    Ok <| EFloat (l + r)

                _ ->
                    Err "Type mismatch in kernel call _Basics_add"

        _ ->
            Err <| "Unknown kernel call " ++ name
